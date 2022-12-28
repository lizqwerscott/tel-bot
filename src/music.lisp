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

;; (defvar *temp* (search-song "我的悲伤是水做的"))

(defun make-file (name extension path)
  (merge-pathnames (format nil "~A.~A" name extension)
                   path))

(defun download-song (song &optional (path (ensure-directories-exist
                                            (merge-pathnames "songs/"
                                                             (get-data-dir)))))
  (let ((finish-path (make-file (assoc-value song "name")
                                "mp3"
                                path)))
    (when (not (probe-file finish-path))
      (let ((song-path (make-file (format nil "~A-last" (assoc-value song "name"))
                                  "mp3"
                                  path))
            (cover-path (make-file (assoc-value song "name")
                                   "jpg"
                                   path)))
        (run-shell
         (format nil
                 "curl -L ~A -o ~A"
                 (assoc-value song "url")
                 song-path))
        (run-shell
         (format nil
                 "curl -L ~A -o ~A"
                 (assoc-value song "picUrl")
                 cover-path))
        (run-shell
         (format nil
                 "lame --ti ~A ~A ~A"
                 cover-path
                 song-path
                 finish-path))
        (delete-file song-path)
        (delete-file cover-path)))
    (list (truename finish-path)
          (assoc-value song "name")
          (car (assoc-value song "person")))))

(defcommand
    (:music "搜索网易云的歌曲" chat text)
    (handler-case
        (let ((res (search-song (str:trim text))))
          (reply-text "正在寻找歌曲中......")
          (if res
              (dolist (song (mapcar #'download-song res))
                (reply-audio (first song) (second song) (third song))
                (sleep 0.5))
              (reply "没有找到任何歌曲哟")))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
