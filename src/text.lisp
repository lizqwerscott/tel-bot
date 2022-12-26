(defpackage :tel-bot.text
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :local-time :cl-telegram-bot)
  (:export
   :get-random-text
   :zaoan
   :wanan))
(in-package :tel-bot.text)

(defvar *tian-address* "api.tianapi.com")
(defvar *tianx-key* "f07a432f84956febe20375736114244e")

(defun handle-tianx (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "newslist")
      (let ((errormsg (format nil
                             "[error][tianx]:~A"
                             (assoc-value data "msg"))))
        (format t "~A~%" errormsg)
        (error errormsg))))

(defun get-zw (path)
  (assoc-value (car
                (handle-tianx
                 (web-get *tian-address*
                          (format nil "~A/index" path)
                          :args `(("key" . ,*tianx-key*))
                          :jsonp t)))
               "content"))

(defun zaoan ()
  (get-zw "zaoan"))

(defun wanan ()
  (get-zw "wanan"))
(handler-case
      (reply (get-random-text "joke"))
    (error (c)
))
;; joke sao love
(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defcommand
    (:love "发送一段情话" chat text)
    (declare (ignorable text))
    (handler-case
      (let ((text (get-random-text "love")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:sao "发送一段骚话" chat text)
    (declare (ignorable text))
    (handler-case
      (let ((text (get-random-text "sao")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:joke "发送一段笑话" chat text)
    (declare (ignorable text))
    (handler-case
      (let ((text (get-random-text "joke")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
