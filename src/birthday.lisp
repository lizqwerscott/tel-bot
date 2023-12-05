(defpackage :tel-bot.birthday
  (:import-from :lzputils.json :assoc-value)
  (:use
   :common-lisp
   :local-time
   :tel-bot.bot
   :tel-bot.web
   :tel-bot.head
   :easy-config)
  (:export
   :get-today-anime-birthday))
(in-package :tel-bot.birthday)

(defvar *birthday-url* (get-config "birthday-address"))

(defun handle-birthday (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "data")
      (let ((errormsg (format nil
                              "[error][birthday]:~A"
                              (assoc-value data "msg"))))
        (log:error "~A~%" errormsg)
        (error errormsg))))

(defun get-anime-birthday (month day)
  "获取 month 月 day 日 的动漫人物的生日"
  (handle-birthday
   (web-get *birthday-url*
            "animebirthday"
            :args `(("month" . ,month)
                    ("day" . ,day))
            :jsonp t)))

(defun first-n (n lst)
  (subseq lst 0 (min (length lst) n)))

(defun join-with-newline (lst)
  (format nil
          (str:join "~%"
                    lst)))

(defun get-today-anime-birthday ()
  "获取今天的动漫人物生日列表"
  (let ((month (timestamp-month (today)))
        (day (timestamp-day (today))))
    (handler-case
        (join-with-newline
         (mapcar #'(lambda (birthday)
                     (format nil
                             "[~A](~A)"
                             (assoc-value birthday "name")
                             (assoc-value birthday "url")))
                 (first-n 4
                          (get-anime-birthday month day))))
      (error (c)
        (log:error "[get anime birthday]: ~A~%" c)))))

(in-package :cl-user)
