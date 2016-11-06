;; (progn
;;   (ql:quickload 'cl-irc)
;;   (ql:quickload 'drakma)
;;   (ql:quickload 'cl-json)
;;   (ql:quickload 'flexi-streams))

(progn
  (load "./bundle/bundle.lisp")
  (cl:require :cl-irc)
  (cl:require :drakma)
  (cl:require :cl-json)
  (cl:require :flexi-streams))

(defpackage :midymidybot
  (:use :cl :cl-user :cl-irc :irc :drakma :json)
  (:export :bot-start
           :bot-shutdown))

(in-package :midymidybot)

(defparameter *irc-channel* "#midymidybot")

(defparameter *tg-auth-str*
  (with-open-file (stream "./account_tg")
    (read-line stream)))

(defparameter *tg-chat-id* -122773250)
(defparameter *tg-bot-id* 258812230)

(defparameter *irc-connection* nil)
(defvar *stdout* *standard-output*)
(defvar *ping-semaphore* (sb-thread:make-semaphore))
(defparameter *irc-read-loop* nil)
(defvar *tg-message-sender*)

(defun time-str ()
  (multiple-value-bind
        (ss mm hh d m)
      (get-decoded-time)
    (format nil "[~A-~A ~2,'0D:~2,'0D:~2,'0D]"
            m d hh mm ss)))

(defvar *log-out* *stdout*)
(defun logging (msg &rest args)
  (apply #'format
         (append
          (list *log-out*
                (concatenate 'string
                             (time-str) " " msg "~%"))
          args)))

(defun exit ()
  (sb-ext:exit))

(defun make-str ()
  (make-array '(0) :element-type 'character
              :fill-pointer 0 :adjustable t))

(defun correct-cljson-surrogate-pairs (wrong-string)
  (with-output-to-string (out)
    (let ((len (length wrong-string)))
      (dotimes (i len)
        (let* ((char1 (aref wrong-string i))
               (c1 (char-code char1)))
          (if (not (and (>= c1 #xD800)
                        (<= c1 #xDBFF)))
              (write-char char1 out)
              (block nil
                (if (>= (1+ i) len)
                    (progn
                      (logging "Corrector: invalid input, A8BSP")
                      (return "[Invalid JSON, code: LXEA6FD]"))
                    (incf i))
                (let ((c2 (char-code (aref wrong-string i))))
                  (write-char
                   (code-char
                    (+ #x10000
                       (ash (logand #x03FF c1) 10)
                       (logand #x03FF c2)))
                   out)))))))))
;; (correct-cljson-surrogate-pairs
;;  (with-input-from-string
;;      (stream "\"你好\\uD83D\\uDE03吼啊\"")
;;    (cl-json:decode-json stream)))

(defmacro tryto (&rest body)
  `(handler-case (progn ,@body)
     (condition () :err)))

(defun msg-user (msg)
  (source msg))

(defun is-privmsg? (msg)
  (string= "PRIVMSG" (command msg)))

(defun msg-channel (msg)
  (car (arguments msg)))

(defun msg-body (msg)
  (cadr (arguments msg)))

(defun msgstr-irc->tg (msg)
  (concatenate 'string (msg-user msg) ": " (msg-body msg)))

(defun send-irc-message (str)
  (privmsg *irc-connection* *irc-channel*
           (correct-cljson-surrogate-pairs str)))

(defvar *irc-message-pool-head* (list :head))
(defvar *irc-message-pool-tail* *irc-message-pool-head*)
(defvar *irc-message-pool-lock*
  (sb-thread:make-mutex
   :name "irc message pool lock"))

(defun push-msg-pool (str)
  (sb-thread:with-mutex (*irc-message-pool-lock*)
    (let ((new (list str)))
      (setf (cdr *irc-message-pool-tail*) new)
      (setf *irc-message-pool-tail* new))))

(defun irc-message-hook (msg)
  (if (string= *irc-channel*
               (msg-channel msg))
      (funcall *tg-message-sender*
               (msgstr-irc->tg msg))
      nil))

;; supress warning
(defun bot-halt ())
(defun send-tg-message (str) str)
(defun irc-check-connection ())
(defun irc-reconnect (&optional (callback nil))
  (tryto (remove-all-hooks *irc-connection*))
  (tryto (quit *irc-connection*))
  (tryto (sb-thread:terminate-thread *irc-read-loop*))
  (sleep 5)
  (logging "Connecting to IRC")
  (setf *irc-connection*
        (connect :nickname "MidyMidyTGBot"
                 :server "irc.freenode.net"))
  (logging "JOIN CHANNEL")
  (join *irc-connection* *irc-channel*)
  (logging "ADD HOOKS")
  (add-hook *irc-connection*
            'irc::irc-privmsg-message
            #'irc-message-hook)
  (add-hook *irc-connection*
            'irc::irc-pong-message
            (lambda (msg)
              (declare (ignore msg))
              (sb-thread:signal-semaphore
               *ping-semaphore*)))
  (logging "ACTIVATE IRC-READ-LOOP")
  (setf *irc-read-loop*
        (sb-thread:make-thread
         (lambda ()
           (read-message-loop *irc-connection*))
         :name "IRC-READ-LOOP"))
  ;;(start-background-message-handler *irc-connection*)
  (if callback
      (funcall callback)))

(defun clear-msg-pool-f ()
  (if (cdr *irc-message-pool-head*)
      (handler-case
          (progn
            (send-irc-message
             (cadr *irc-message-pool-head*))
            (setf (cdr *irc-message-pool-head*)
                  (cddr *irc-message-pool-head*))
            (clear-msg-pool-f))
        (condition ()
          (logging "clear-msg-pool-f: Cannot send message!")))))

(defun irc-shutdown ()
  (tryto
   (part *irc-connection* *irc-channel* "Bot shutdown"))
  (tryto (quit *irc-connection* "leaving")))

(defun irc-check-connection ()
  (tryto (ping *irc-connection* *irc-channel*))
  (if (sb-thread:wait-on-semaphore
       *ping-semaphore* :timeout 20)
      (progn (setf *ping-semaphore* (sb-thread:make-semaphore))
             t)
      (progn (setf *ping-semaphore* (sb-thread:make-semaphore))
             nil)))

;;;; -----------------------------------------

(defun tg-request (method-name &optional parameters)
  (let ((http-method (if parameters :post :get)))
    (flexi-streams:octets-to-string
     (http-request
      (concatenate 'string
                   "https://api.telegram.org/"
                   *tg-auth-str* "/"
                   method-name)
      :connection-timeout 100
      :method http-method
      :content-type "application/json"
      :content (if parameters
                   (with-output-to-string (stream)
                     (encode-json parameters stream))))
     :external-format '(:utf-8 :eol-style :crlf))))

(defun decoded-tg-request (method-name &optional parameters)
  (with-input-from-string
      (stream (tg-request method-name parameters))
    (decode-json stream)))

(defun jget (tag decoded-json)
  (dolist (i decoded-json nil)
    (if (eq (car i) tag)
        (return (cdr i)))))

(defun tg-is-message? (update)
  (if (jget :message update) t nil))

(defun tg-is-sticker? (update)
  (if (jget :sticker (jget :message update)) t nil))

(defun tg-is-reply? (update)
  (if (jget :reply--to--message
            (jget :message update)) t nil))

(defun tg-is-our-chat? (update)
  (= *tg-chat-id*
     (jget :id (jget :chat (jget :message update)))))

(defun msgstr-tg->irc-list (update)
  (let ((first-name (jget :first--name
                          (jget :from (jget :message update))))
        (text (jget :text (jget :message update)))
        (str (make-str))
        (lst nil))
    (labels ((make-msg (str)
               (concatenate 'string first-name ": " str)))
      (loop for c across text do
           (if (not (equal #\Newline c))
               (with-output-to-string (out str)
                 (write-char c out))
               (progn (push (make-msg str) lst)
                      (setf str (make-str)))))
      (if (= 0 (length str))
          nil
          (push (make-msg str) lst))
      (reverse lst))))

(defun remove-newline (str)
  (with-output-to-string (out)
    (loop for c across str do
         (write-char (if (equal #\Newline c)
                         #\Space c)
                     out))))

(defun tg-update-replier-id (update)
  (jget :id
        (jget :from
              (jget :reply--to--message
                    (jget :message update)))))

(defun tg-update-replier-first-name (update)
  (jget :first--name
        (jget :from
              (jget :reply--to--message
                    (jget :message update)))))

(defun msgstr-tgreply->irc-list (update)
  (let* ((too-long 40)
         (text-lst (msgstr-tg->irc-list update))
         (reply-to
          (remove-newline
           (jget :text
                 (jget :reply--to--message
                       (jget :message update))))))
    (if (= (tg-update-replier-id update) *tg-bot-id*)
        nil
        (setf reply-to
              (concatenate
               'string
               (tg-update-replier-first-name update)
               ": " reply-to)))
    (let ((reply-to-sub
           (if (> (length reply-to) too-long)
               (concatenate 'string
                            (subseq reply-to 0 too-long)
                            "......")
               reply-to)))
      (setf (car text-lst)
            (concatenate 'string
                         "Re: [" reply-to-sub "] "
                         (car text-lst)))
      text-lst)))

(defun msgstr-tgsticker->irc (update)
  (let ((first-name
         (jget :first--name
               (jget :from (jget :message update))))
        (emoji
         (jget :emoji
               (jget :sticker
                     (jget :message update)))))
    (concatenate 'string
                 first-name ": "
                 emoji emoji emoji)))

(defun send-tg-message (str)
  (decoded-tg-request
   "sendMessage"
   `(("chat_id" . ,*tg-chat-id*)
     ("text" . ,str))))
(setf *tg-message-sender* #'send-tg-message)

(defun process-tg-msg (update)
  (let ((msg-lst
         (cond
           ((tg-is-sticker? update) `(,(msgstr-tgsticker->irc update)))
           ((tg-is-reply? update) (msgstr-tgreply->irc-list update))
           ((tg-is-message? update) (msgstr-tg->irc-list update)))))
    (dolist (msg msg-lst)
      (handler-case
          (send-irc-message msg)
        (condition (e)
          (progn
            (logging
             "TG->IRC: can not send message! Reason: ~S" e)
            (push-msg-pool msg)))))))

(defun tg-getupdate-loop ()
  "Need a `overheat' protection"
  (let ((offset 0))
    (loop
       (handler-case
           (let* ((response (decoded-tg-request
                             "getUpdates"
                             `(("offset" . ,offset)
                               ("timeout" . 17))))
                  (result (jget :result response)))
             (mapl (lambda (result-lst)
                     (if (null (cdr result-lst))
                         (setf offset
                               (1+ (jget :update--id
                                         (car result-lst)))))
                     (process-tg-msg (car result-lst)))
                   result))
         (condition (e)
           (progn (logging "TG-LOOP in trouble: ~S!" e)
                  ;; prevent loop overheat
                  (sleep 2)))))))

;;;;------------------------------------------------

(defparameter *tg-loop* nil)
(defparameter *irc-watcher* nil)
(defvar *irc-reconnect-counter* 0)

(defun creat-watcher ()
  (format *stdout* "Create IRC watcher")
  (setf *irc-watcher*
        (sb-thread:make-thread
         (lambda ()
           (loop
              (sleep 60)
              (logging "Checking IRC Connection ...")
              (if (irc-check-connection)
                  (progn
                    (logging "Connection OK!")
                    (logging "Trying to clear Msg Pool")
                    (clear-msg-pool-f)
                    (setf *irc-reconnect-counter* 0))
                  (progn
                    (logging "FAILED! RECONNECTING!")
                    (incf *irc-reconnect-counter*)
                    (if (> 20 *irc-reconnect-counter*)
                        (handler-case (progn (irc-reconnect)
                                             (clear-msg-pool-f))
                          (condition (e)
                            (logging "Having Trouble: ~S" e)))
                        (progn (logging "Give up, Bot halt!")
                               (bot-halt)))))))
         :name "IRC-WATCHER")))

(defun bot-start ()
  (irc-reconnect)
  (logging "Creat TG LOOP")
  (setf *tg-loop* (sb-thread:make-thread #'tg-getupdate-loop
                                         :name "TG-LOOP"))
  (sleep 10)
  (creat-watcher))

(defun bot-halt ()
  (tryto (sb-thread:terminate-thread *tg-loop*))
  (tryto (sb-thread:terminate-thread *irc-watcher*))
  (tryto (irc-shutdown)))

