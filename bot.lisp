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
           :bot-halt))

(in-package :midymidybot)
(defvar *log-out* *standard-output*)
(load "./utils.lisp")

(defparameter *irc-channel* "#midymidybot")

(defparameter *tg-auth-str*
  (with-open-file (stream "./account_tg")
    (read-line stream)))

(defparameter *tg-chat-id* -122773250)
(defparameter *tg-bot-id* 258812230)

(defparameter *irc-connection* nil)
(defvar *ping-semaphore* (sb-thread:make-semaphore))
(defparameter *irc-read-loop* nil)
(defvar *tg-message-sender*)

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
  (tryto (quit *irc-connection* "leaving"))
  (tryto (sb-thread:terminate-thread *irc-read-loop*)))

(defun irc-check-connection ()
  (let ((t1 (get-internal-real-time)))
    (tryto (ping *irc-connection* *irc-channel*))
    (if (sb-thread:wait-on-semaphore
         *ping-semaphore* :timeout 20)
        (let ((time-elapse
               (float (/ (- (get-internal-real-time) t1)
                         internal-time-units-per-second))))
          (setf *ping-semaphore*
                (sb-thread:make-semaphore))
          time-elapse)
        (progn (setf *ping-semaphore*
                     (sb-thread:make-semaphore))
               nil))))

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

(defun tg-is-text? (update)
  (if (jget :text (jget :message update)) t nil))

(defun tg-is-sticker? (update)
  (if (jget :sticker (jget :message update)) t nil))

(defun tg-is-reply? (update)
  (if (jget :reply--to--message
            (jget :message update)) t nil))

(defun tg-is-our-chat? (update)
  (= *tg-chat-id*
     (jget :id (jget :chat (jget :message update)))))

(defun tg-is-photo? (update)
  (if (jget :photo
            (jget :message update)) t nil))

(defun tg-get-photo-id (update)
  (let ((file-lst (jget :photo
                        (jget :message update))))
    (setf file-lst ;; must less than 400KB
          (remove-if (lambda (file)
                       (> (jget :file--size file)
                          (* 400 1024)))
                     file-lst))
    (jget :file--id
          (car
           (sort file-lst
                 (lambda (a b)
                   (> (jget :file--size a)
                      (jget :file--size b))))))))

(defun tg-get-file-path (file-id)
  (jget :file--path
        (jget :result
              (decoded-tg-request
               "getFile" `(("file_id" . ,file-id))))))
;; (tg-get-file-path (tg-get-photo-id up))
(defun tg-download-file (file-path)
  (http-request
   (concatenate 'string
                "https://api.telegram.org/file/"
                *tg-auth-str* "/"
                file-path)))

(defun tg-sender-first-name (update)
  (jget :first--name
        (jget :from
              (jget :message update))))

(defun msgstr-tg->irc-list (update)
  (let ((first-name (tg-sender-first-name update))
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

(defun tg-update-repliee-id (update)
  (jget :id
        (jget :from
              (jget :reply--to--message
                    (jget :message update)))))

(defun tg-update-repliee-first-name (update)
  (jget :first--name
        (jget :from
              (jget :reply--to--message
                    (jget :message update)))))

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

(defun msgstr-tgphoto->irc (update)
  (let* ((img-vector (tg-download-file
                      (tg-get-file-path
                       (tg-get-photo-id update))))
         (url (with-input-from-string
                  (stream (upload-binary-file img-vector))
                (read-line stream)))
         (caption (jget :caption (jget :message update)))
         (first-name
          (jget :first--name
                (jget :from (jget :message update)))))
    (concatenate 'string
                 first-name ": "
                 "[ " url " ] " (if caption caption ""))))

(defun msgstr-tgreply->irc (update)
  "Transform only reply refer to irc, not text"
  (let* ((too-long     40)
         (dummy-update `(,(cons
                           :message
                           (jget
                            :reply--to--message
                            (jget :message update)))))
         (photo
          (if (tg-is-photo? dummy-update)
              "[ photo ]" nil))
         (file nil)
         (sticker
          (if (tg-is-sticker? dummy-update)
              (msgstr-tgsticker->irc dummy-update)
              nil))
         (caption (jget :caption
                        (jget :message dummy-update)))
         (text (jget :text
                     (jget :message dummy-update)))
         (reply-to-text
          (remove-newline
           (with-output-to-string (out)
             (if photo (write-line photo out))
             (if file (write-line file out))
             (if caption (write-line caption out))
             (if sticker (write-line sticker out))
             (if text (write-string text out))
             out))))
    (if (= (tg-update-repliee-id update)
           *tg-bot-id*)
        (setf reply-to-text
              (concatenate 'string
                           "(IRC) "
                           reply-to-text))
        (setf reply-to-text
              (concatenate
               'string
               (tg-update-repliee-first-name update)
               ": " reply-to-text)))
    (let ((reply-to-text-cut
           (if (> (length reply-to-text) too-long)
               (concatenate 'string
                            (subseq reply-to-text
                                    0 too-long)
                            "......")
               reply-to-text)))
      (concatenate 'string
                   (tg-sender-first-name update)
                   ": "
                   "[ Re: " reply-to-text-cut " ]"))))

(defun send-tg-message (str)
  (decoded-tg-request
   "sendMessage"
   `(("chat_id" . ,*tg-chat-id*)
     ("text" . ,str))))
(setf *tg-message-sender* #'send-tg-message)

(defun process-tg-msg (update)
  (if (tg-is-message? update) ; don't care about other data
      ;; one-line: reply, photo, file, sticker
      ;; multi-line: text
      (let* ((reply    (if (tg-is-reply? update)
                           (msgstr-tgreply->irc update)))
             (photo    nil) ;; deal with it later
             (file     nil) ;; deal with it later
             (sticker  (if (tg-is-sticker? update)
                           (msgstr-tgsticker->irc update)))
             (text-lst (if (tg-is-text? update)
                           (msgstr-tg->irc-list update)))
             (result   nil))
        (handler-case
            (progn
              (setf photo (if (tg-is-photo? update)
                              (msgstr-tgphoto->irc update)))
              (setf file nil))
          (condition (e)
            (logging
             "process-tg-msg: trouble on uploading file: ~A"
             e)))
        (setf result
              (append `(,reply ,photo ,file ,sticker)
                      text-lst))
        (if (not (or reply photo file sticker text-lst))
            (setf result `(,(concatenate
                             'string
                             (tg-sender-first-name update)
                             ": [ other media ]"))))
        (dolist (i result)
          (handler-case
              (if i (send-irc-message i))
            (condition (e)
              (progn
                (logging
                 "process-tg-msg: trouble of sending msg: ~A"
                 e))))))))

(defun tg-sort-result (result)
  (sort result
        (lambda (a b)
          (< (jget :update--id a)
             (jget :update--id b)))))

(defun tg-getupdate-loop ()
  "Need a `overheat' protection"
  (let ((offset 0))
    (loop
       (handler-case
           (let* ((response (decoded-tg-request
                             "getUpdates"
                             `(("offset" . ,offset)
                               ("timeout" . 17))))
                  (result (tg-sort-result
                           (jget :result response))))
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
  (logging "Create IRC watcher")
  (setf *irc-watcher*
        (sb-thread:make-thread
         (lambda ()
           (loop
              (sleep 60)
              (logging "Checking IRC Connection ...")
              (let ((delay (irc-check-connection)))
                (if delay
                    (progn
                      (logging "OK! Ping delay: ~As" delay)
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
                                 (bot-halt))))))))
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

