(defpackage :tel-bot.text
  (:import-from :lzputils.json :assoc-value)
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :local-time :cl-telegram-bot)
  (:export
   :get-random-text
   :zaoan
   :wanan
   :holidays
   :english-line))
(in-package :tel-bot.text)

(defvar *tian-address* "api.tianapi.com")
(defvar *tianx-key* "f07a432f84956febe20375736114244e")

(defun handle-tianx (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "newslist")
      (let ((errormsg (format nil
                              "[error][tianx]:~A"
                              (assoc-value data "msg"))))
        (log:error "~A~%" errormsg)
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

;; joke sexy love
(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/text/~A" command)))

(defun holidays ()
  (car
   (handle-tianx
    (web-get *tian-address*
             "jiejiari/index"
             :args `(("key" . ,*tianx-key*)
                     ("date" . ,(today-format))
                     ("mode" . 1))
             :jsonp t))))

(defun english-line ()
  (car
   (handle-tianx
    (web-get *tian-address*
             "everyday/index"
             :args `(("key" . ,*tianx-key*))
             :jsonp t))))

(defun get-story (&key (story-type 2))
  "type 故事类型，成语1、睡前2、童话3、寓言4, word 故事标题"
  (car
   (handle-tianx
    (web-get *tian-address*
             "story/index"
             :args `(("key" . ,*tianx-key*)
                     ("storytype" . ,story-type))
             :jsonp t))))

(defcommand
    (:love "发送一段情话" chat text)
    (declare (ignorable text))
    (handler-case
        (let ((text (get-random-text "love")))
          (reply text))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:sexy "发送一段骚话" chat text)
    (declare (ignorable text))
    (handler-case
        (let ((text (get-random-text "sexy")))
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

(defun handle-story (story)
  (format nil
          "~A~%~A"
          (assoc-value story "title")
          (assoc-value story "content")))

(defcommand
    (:story "发送一段故事(参数: 成语, 睡前, 童话, 寓言)" chat text)
    (handler-case
        (reply
         (let ((match-res (cond
                            ((string= text "成语")
                             1)
                            ((string= text "睡前")
                             2)
                            ((string= text "童话")
                             3)
                            ((string= text "寓言")
                             4)
                            (t
                             5))))
           (if (string= text "")
               (handle-story
                (get-story))
               (if (= match-res 5)
                   "只支持成语、睡前、童话、寓言"
                   (handle-story
                    (get-story :story-type match-res))))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
