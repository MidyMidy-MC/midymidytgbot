(ql:quickload 'cl-irc)
(ql:quickload 'drakma)
(ql:quickload 'cl-json)
(ql:quickload 'flexi-streams)

(defpackage :midymidybot
  (:use :cl :cl-user :cl-irc :irc :drakma :json))

(in-package :midymidybot)

(progn
  (defparameter *irc-channel* "#midymidybot")
  (defparameter *irc-connection* nil)

  (defparameter *msg* nil)

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
    (privmsg *irc-connection* *irc-channel* str))

  (defun irc-shutdown ()
    (part *irc-connection* *irc-channel* "Bot shutdown")
    (quit *irc-connection* "leaving"))
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

  (defun tg-is-our-chat? (update)
    (= *tg-chat-id*
       (jget :id (jget :chat (jget :message update)))))

  (defun msgstr-tg->irc (update)
    (let ((first-name
           (jget :first--name
                 (jget :from (jget :message update))))
          (text
           (jget :text (jget :message update))))
      (concatenate 'string
                   first-name ": "
                   text)))

  (defun send-tg-message (str)
    (decoded-tg-request
     "sendMessage"
     `(("chat_id" . ,*tg-chat-id*)
       ("text" . ,str))))

  (defun tg-getupdate-loop ()
    (let ((offset 0))
      (loop
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
                   (let ((i (car result-lst)))
                     (if (and (tg-is-message? i)
                              (tg-is-our-chat? i))
                         (send-irc-message
                          (msgstr-tg->irc i)))))
                 result)))))

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
  )
;;;;------------------------------------------------

(defparameter *tg-loop* nil)
(setf *tg-loop* (sb-thread:make-thread #'tg-getupdate-loop))

(progn
  (setf *irc-connection*
        (connect :nickname "MidyMidyTGBot"
                 :server "irc.freenode.net"))
  (join *irc-connection* *irc-channel*)
  (add-hook *irc-connection* 'irc::irc-privmsg-message
            (lambda (msg)
              (send-tg-message
               (msgstr-irc->tg msg))))
  (start-background-message-handler *irc-connection*))

(ping *irc-connection* "irc.freenode.net")

