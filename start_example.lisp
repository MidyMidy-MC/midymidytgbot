(in-package :cl-user)

(load "bot.lisp")

(setf midymidybot:*log-file* #p"tgbot.log")

(in-package :midymidybot)

(defparameter *bot0* nil)

(setf
 *bot0*
 (bot-start
  '((:name . "midymidy")
    (:irc . ((:username . "MidyMidyTGBot")
             (:channel . "#MidyMidymc")
             (:server . "irc.freenode.net")
             (:port . 7000)
             (:passwd . "abcdefg")
             (:znc . nil)))
    (:tg . ((:bot-id . 123456)
            (:bot-token
             . "bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11")
            (:chat-id . -233333333333333))))))

;;(midymidybot:bot-halt bot)
