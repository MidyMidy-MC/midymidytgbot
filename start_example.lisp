(in-package :cl-user)

(load "bot.lisp")

(defvar
    bot
  (midymidybot:bot-start
   '((:irc . ((:username . "testbot")
              (:channel . "#lisp")))
     (:tg . ((:bot-id . 23333333)
             (:bot-token
              . "bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11")
             (:chat-id . -6666666))))))

;;(midymidybot:bot-halt bot)
