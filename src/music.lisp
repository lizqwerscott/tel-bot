(defpackage tel-bot.music
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :cl-telegram-bot)
  (:export
   ))
(in-package :tel-bot.music)

(defvar *address* "192.168.3.127:3000")

(defun handle-data (data &optional (result-name "result"))
  (let ((code (assoc-value data "code")))
    (if (= code 200)
        (assoc-value data result-name)
        (format t "[Error][song]:~A~%" data))))

(defun check-music (id)
  (assoc-value (web-get *address*
                        "check/music"
                        :args `(("id" . ,id))
                        :jsonp t)
               "success"))

(defun get-url (id)
  (assoc-value (car
                (handle-data (web-get *address*
                                      "song/url/vr"
                                      :args `(("id" . ,id))
                                      :jsonp t)
                             "data"))
               "url"))

(defun get-info-song (result)
  (format t "len:~A~%" (assoc-value result "songCount"))
  (mapcar #'(lambda (song)
              (list (assoc-s song "name")
                    (let ((id (assoc-value song "id")))
                      `("url" . ,(when (check-music id)
                                   (get-url id))))
                    (assoc-s (assoc-value song "al")
                             "picUrl")
                    `("person" . ,(mapcar #'(lambda (artist)
                                              (assoc-value artist
                                                           "name"))
                                          (assoc-value song "ar")))))
          (assoc-value result "songs")))


(defun search-song (keywords &optional (limit 1))
  (get-info-song
   (handle-data
    (web-get *address*
             "cloudsearch"
             :args `(("keywords" . ,keywords)
                     ("type" . 1)
                     ("limit" . ,limit))
             :jsonp t))))

(defcommand
    (:music "搜索网易云的歌曲" chat text)
    (handler-case
        (let ((res (search-song (str:trim text))))
          (if res
              (dolist (song res)
                (let ((url (assoc-value song "url")))
                  (when url
                    (send-text chat
                               (format nil
                                       "标题: ~A~%歌手:~{~A~}"
                                       (assoc-value song "name")
                                       (assoc-value song "person")))
                    (send-picture chat
                                  (assoc-value song "picUrl"))
                    (send-audio chat
                                url))
                  (sleep 1)))
              (reply "没有找到任何歌曲哟")))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
