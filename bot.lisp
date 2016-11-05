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

(progn
  (defparameter *irc-channel* "#midymidybot")
  (defparameter *irc-connection* nil)

  (defparameter *pong* 0)
  (setf *pong* 0)

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
                (progn
                  (if (>= (1+ i) len)
                      (error "Unfinished input")
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

  (defvar *tg-message-sender*)

  (defun irc-reconnect (&optional (try 0) (callback nil))
    (if (> try 50)
        (progn
          (format t "FATAL ERROR: CAN NOT CONNECT TO IRC~%")
          (send-tg-message "can not connect to IRC, BOT HALT!")
          (format t "BOT HALT~%")
          (bot-halt)
          (error "BOT HALT!"))
        (progn
          (tryto (remove-all-hooks *irc-connection*))
          (tryto (quit *irc-connection*))
          (sleep 5)
          (setf *irc-connection*
                (connect :nickname "MidyMidyTGBot"
                         :server "irc.freenode.net"))
          (join *irc-connection* *irc-channel*)
          (add-hook *irc-connection*
                    'irc::irc-privmsg-message
                    (lambda (msg)
                      (funcall *tg-message-sender*
                               (msgstr-irc->tg msg))))
          (add-hook *irc-connection*
                    'irc::irc-pong-message
                    (lambda (msg)
                      (setf *pong* (received-time msg))))
          (start-background-message-handler *irc-connection*)
          (if callback
              (funcall callback)))))

  (defun clear-msg-pool-f (&optional (try 0))
    (if (cdr *irc-message-pool-head*)
        (handler-case
            (progn
              (send-irc-message
               (cadr *irc-message-pool-head*))
              (setf (cdr *irc-message-pool-head*)
                    (cddr *irc-message-pool-head*))
              (clear-msg-pool-f))
          (condition ()
            (progn (format t "Error: Cannot send message!~%")
                   (irc-reconnect
                    (1+ try)
                    (lambda ()
                      (clear-msg-pool-f (1+ try)))))))))

  (defun irc-shutdown ()
    (tryto
     (part *irc-connection* *irc-channel* "Bot shutdown"))
    (tryto (quit *irc-connection* "leaving")))

  (defun irc-check-connection ()
    (let ((ping-time (get-universal-time)))
      (ping *irc-connection* *irc-channel*)
      (sleep 20)
      (let ((pong-time *pong*))
        (if (> (- pong-time ping-time) 20)
            (progn (tryto (send-tg-message "Reconnecting to IRC"))
                   (tryto (irc-reconnect 1)))
            nil))))
  )
;;;; -----------------------------------------

(progn
  (defparameter *tg-auth-str*
    (with-open-file (stream "./account_tg")
      (read-line stream)))
  (defparameter *tg-chat-id* -122773250)

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
    (jget :message update))

  (defun tg-is-sticker? (update)
    (if (jget :sticker (jget :message update))
        t nil))

  (defun tg-is-our-chat? (update)
    (= *tg-chat-id*
       (jget :id (jget :chat (jget :message update)))))

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

  (defun msgstr-tg->irc-list (update)
    (let ((first-name (jget :first--name
                            (jget :from (jget :message update))))
          (text (jget :text (jget :message update)))
          (str (make-str))
          (lst nil))
      (loop for c across text do
           (if (not (equal #\Newline c))
               (with-output-to-string (out str)
                 (write-char c out))
               (progn (push (concatenate 'string first-name ": " str)
                            lst)
                      (setf str (make-str)))))
      (if (= 0 (length str))
          nil
          (push str lst))
      (reverse lst)))

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
             ((tg-is-message? update) (msgstr-tg->irc-list update)))))
      (dolist (msg msg-lst)
        (handler-case
            (send-irc-message msg)
          (condition (e)
            (format
             t
             "WARNING, can not send message! Reason: ~S" e)
            (push-msg-pool msg)
            (irc-reconnect
             0
             (lambda ()
               (clear-msg-pool-f 1))))))))

  (defun tg-getupdate-loop ()
    "Need a `overheat' protection"
    (let ((offset 0))
      (loop
         (handler-case
             (let* ((response (decoded-tg-request
                               "getUpdates"
                               `(("offset" . ,offset)
                                 ("timeout" . 60))))
                    (result (jget :result response)))
               (mapl (lambda (result-lst)
                       (if (null (cdr result-lst))
                           (setf offset
                                 (1+ (jget :update--id
                                           (car result-lst)))))
                       (process-tg-msg (car result-lst)))
                     result))
           (condition (e) (format t "Error: ~S!\n" e))))))

  )
;;;;------------------------------------------------

(defparameter *tg-loop* nil)
(defparameter *irc-watcher* nil)

(defun bot-start ()
  (irc-reconnect)
  (setf *irc-watcher*
        (sb-thread:make-thread
         (lambda ()
           (loop
              (sleep 60)
              (irc-check-connection)))))
  (setf *tg-loop* (sb-thread:make-thread #'tg-getupdate-loop)))

(defun bot-halt ()
  (tryto (sb-thread:terminate-thread *tg-loop*))
  (tryto (sb-thread:terminate-thread *irc-watcher*))
  (tryto (irc-shutdown)))

