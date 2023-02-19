(defpackage tel-bot.picture
  (:import-from :lzputils.json :assoc-value)
  (:import-from :lzputils.used :if-return)
  (:use :cl :tel-bot.head :tel-bot.web :tel-bot.bot :cl-telegram-bot)
  (:export
   :random-picture))

(in-package :tel-bot.picture)

(defun loli-get (&optional (switch "acg"))
  "Random get picture from LoliApi.
switch have two parameter: (acg) (bg)"
  (handler-case
      (str:trim
       (web-get-url
        (format nil "https://www.loliapi.com/~A/?type=url" switch)))
    (error (c)
      (format t "[loli get picture Error]: ~A~%" c)
      nil)))

(defun hanxing-get ()
  (handler-case
      (let ((res (web-get-url "http://api.hanximeng.com/ranimg/api.php?type=json"
                              :jsonp t)))
        (when (= 200 (assoc-value res "code"))
          (assoc-value res '("info" "url"))))
    (error (c)
      (format t "[hanxing get picture Error]: ~A~%" c)
      nil)))

(defun syxz-get ()
  (handler-case
      (let ((res (web-get-url "https://img.xjh.me/random_img.php?&return=json"
                          :jsonp t)))
        (when (= 200 (assoc-value res "result"))
          (format nil "https:~A" (assoc-value res "img"))))
    (error (c)
      (format t "[syxz get picture Error]: ~A~%" c)
      nil)))

(defun vvhan-get ()
  (handler-case
      (let ((res (web-get-url "https://api.vvhan.com/api/acgimg?type=json"
                              :jsonp t)))
        (when (assoc-value res "success")
          (assoc-value res "imgurl")))
    (error (c)
      (format t "[syxz get picture Error]: ~A~%" c)
      nil)))

(defun random-picture (&optional (webs (list #'loli-get #'vvhan-get #'syxz-get #'hanxing-get)))
  (when (listp webs)
    (let ((select (random-select-list webs)))
      (if-return (funcall select)
        (random-picture (remove select webs))))))

(defcommand
    (:picture "随机获取一张图片" chat text)
    (reply-picture (random-picture)))

(in-package :cl-user)
