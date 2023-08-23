(defpackage tel-bot.music
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :cl-telegram-bot :lzputils.json :lzputils.used :easy-config)
  (:export
   ))
(in-package :tel-bot.music)

(defvar *address* "192.168.3.127:3000")

(defun handle-data (data &optional (result-name "result"))
  (let ((code (assoc-value data "code")))
    (if (= code 200)
        (assoc-value data result-name)
        (log:error "[song]: ~A" data))))

(defun check-music (id)
  (assoc-value (web-get *address*
                        "check/music"
                        :args `(("id" . ,id))
                        :jsonp t)
               "success"))

(defun get-url (id)
  (assoc-value (car
                (handle-data (web-get *address*
                                      "song/url/v1"
                                      :args `(("id" . ,id)
                                              ("level" . "standard"))
                                      :jsonp t)
                             "data"))
               "url"))

(defun get-info-song (result)
  (format t "len:~A~%" (assoc-value result "songCount"))
  (mapcar #'(lambda (song)
              (list (assoc-s song "name")
                    (assoc-s song "id")
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


(defun search-song (keywords &optional (limit 3))
  (get-info-song
   (handle-data
    (web-get *address*
             "cloudsearch"
             :args `(("keywords" . ,keywords)
                     ("type" . 1)
                     ("limit" . ,limit))
             :jsonp t))))

;; (defvar *temp* (search-song "我的悲伤是水做的"))

(defun fix-song (cover-path song-path finish-path)
  (handler-case
      (progn
        (run-shell
         (format nil
                 "lame --ti ~A ~A ~A"
                 cover-path
                 song-path
                 finish-path))
        finish-path)
    (error (c)
      (log:error t "[fix-song Error]: ~A~%" c))))

(defun download-song (song &optional (path (ensure-directories-exist
                                            (merge-pathnames "songs/"
                                                             (get-data-path)))))
  (when (assoc-value song "url")
    (let ((finish-path (make-file (assoc-value song "id")
                                  "mp3"
                                  path)))
      (if  (probe-file finish-path)
           (list (truename finish-path)
                 (assoc-value song "name")
                 (car (assoc-value song "person")))
           (let ((song-path (make-file (format nil "~A-last" (assoc-value song "id"))
                                       "mp3"
                                       path))
                 (cover-path (make-file (assoc-value song "id")
                                        "jpg"
                                        path)))
             (when (and (download-url (assoc-value song "url")
                                      song-path)
                        (download-url (assoc-value song "picUrl")
                                      cover-path)
                        (fix-song cover-path song-path finish-path))
               (delete-file song-path)
               (delete-file cover-path)
               (list (truename finish-path)
                     (assoc-value song "name")
                     (car (assoc-value song "person")))))))))

(defcommand
    (:music "搜索网易云的歌曲(寄)" chat text)
    (handler-case
        (let ((res (search-song (str:trim text))))
          (reply-text "正在寻找歌曲中......")
          (format t "songs: ~A~%" res)
          (if res
              (dolist (song (mapcar #'download-song res))
                (when song
                  (reply-audio (first song) (second song) (third song)))
                (sleep 0.5))
              (reply "没有找到任何歌曲哟")))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
