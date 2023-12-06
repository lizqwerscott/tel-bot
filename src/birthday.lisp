(defpackage :tel-bot.birthday
  (:import-from :lzputils.json :assoc-value)
  (:use
   :common-lisp
   :local-time
   :tel-bot.bot
   :tel-bot.web
   :tel-bot.head
   :easy-config
   :cl-telegram-bot)
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

(defun replace-all-alist (lst source)
  (if lst
      (replace-all-alist (cdr lst)
                         (replace-all-l (car (car lst))
                                        (cdr (car lst))
                                        source))
      source))

(defun get-today-anime-birthday (&optional (n 4))
  "获取今天的动漫人物生日列表, n 指获取列表的前几项, 默认为4项, -1 则为全部"
  (let ((month (timestamp-month (today)))
        (day (timestamp-day (today))))
    (handler-case
        (join-with-newline
         (mapcar #'(lambda (birthday)
                     (format nil
                             "[~A](~A)"
                             (replace-all-alist '(("(" . " ")
                                                  (")" . " ")
                                                  ("-" . " "))
                                                (assoc-value birthday "name"))
                             (assoc-value birthday "url")))
                 (if (= n -1)
                     (get-anime-birthday month day)
                     (first-n n
                              (get-anime-birthday month day)))))
      (error (c)
        (log:error "[get anime birthday]: ~A~%" c)))))

(defcommand
    (:birthday "返回今天的动漫人物生日, 参数: n, 获取今日前几项动漫人物(不传默认前10项, -1 为全部)" chat text)
    (handler-case
        (reply-markdown
         (let ((real-text (str:trim text)))
           (let ((n-str (unless (string= real-text "")
                          (first (split-s (str:trim text))))))
             (get-today-anime-birthday
              (if n-str
                  (parse-integer n-str)
                  10)))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
