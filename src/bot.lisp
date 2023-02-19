(defpackage tel-bot.bot
  (:import-from :cl-telegram-bot/message
                :get-current-chat)
  (:import-from :cl-telegram-bot/chat
                :get-chat-id)
  (:import-from :cl-telegram-bot/chat
                :get-chat-by-id)
  (:use :cl :cl-telegram-bot :tel-bot.head :lzputils.json :lzputils.string :lzputils.used)
  (:export

   :create-bot
   :start-bot
   :stop-bot

   :defcommand

   :manager-bot
   :make-manager-bot

   :get-master-chat
   :get-manager-group

   :send-text
   :send-picture
   :send-audio

   :reply-text
   :reply-picture
   :reply-audio))
(in-package :tel-bot.bot)

(defbot manager-bot)

(defvar *bot* nil)

(cl-telegram-bot/network:set-proxy
   (assoc-value (get-configs)
                "proxy"))

(defun create-bot ()
  (setf *bot*
        (make-manager-bot (assoc-value (get-configs)
                                       "botToken")
                          :debug t)))

(defun start-bot ()
  (start-processing *bot*))

(defun stop-bot ()
  (stop-processing *bot*))

;; Help command
(defparameter *command-infos* (make-hash-table :test #'eql))

(defun help ()
  (let ((help-text nil))
    (maphash #'(lambda (command info)
                 (setq help-text
                       (append help-text
                               (list
                                (format nil "/~(~A~): ~A" command info)))))
             *command-infos*)
    (format nil "命令介绍:~%~{~A~%~}" help-text)))

(defmethod on-message ((bot manager-bot) text)
  (let ((words (str:trim text)))
    (format t "message: ~A~%" words)
    (when (start-with-words? words
                             '("初音" "miku" "初音未来" "@kk_manage_bot"))
      (if (and (include-words? text
                             '("会" "懂" "能做"))
             (include-words? text
                           '("什么")))
          (reply (help))))))

(defmethod on-command ((bot manager-bot) (command (eql :help)) text)
  (reply (help)))

(defun add-command-info (command info)
  (setf (gethash command *command-infos*)
        info))

(defmethod on-command ((bot manager-bot) (command (eql :start)) text)
  (declare (ignorable text))
  (reply "Welcome Lisper, you can use /help to get more information for this bot."))

;; (defcommand (:love "发送一段情话" chat text)
;;            (declare (ignorable text))
;;            (reply "hahah"))
;; up marco epxand to this
;; (progn
;;   (add-command-info :love "发送一段情话")
;;   (defmethod on-command ((bot manager-bot) (command (eql :love)) text)
;;     (declare (ignorable text))
;;     (reply "hahah")))

(defmacro defcommand ((name info chat-name text-name) &rest body)
  `(progn
    (add-command-info ,name ,info)
    (defmethod on-command ((bot manager-bot) (command (eql ,name)) ,text-name)
     (let ((,chat-name (get-current-chat)))
       ,@body))))

(defun send-text (chat-id text)
  (cl-telegram-bot/message:send-message *bot*
                                        (if (numberp chat-id)
                                            (get-chat-by-id *bot* chat-id)
                                            chat-id)
                                        text))

(defun send-picture (chat-id url)
  (cl-telegram-bot/message:send-photo *bot*
                                      (if (numberp chat-id)
                                          (get-chat-by-id *bot* chat-id)
                                          chat-id)
                                      url))

(defun send-audio (chat-id audio title performer)
  (if (pathnamep audio)
      (run-shell
       (format nil
               "proxychains4 python ~A ~A ~A ~A '~A' '~A'"
               (merge-pathnames "scripts/send_audio.py"
                                (asdf:system-source-directory :tel-bot))
               (assoc-value (get-configs)
                            "botToken")
               (if (numberp chat-id)
                   chat-id
                   (get-chat-id chat-id))
               audio
               title
               performer))
      (cl-telegram-bot/message:send-audio *bot*
                                          (if (numberp chat-id)
                                              chat-id
                                              (get-chat-by-id chat-id))
                                          url)))

(defun reply-text (text)
  (send-text (get-current-chat) text))

(defun reply-picture (url)
  (send-picture (get-current-chat) url))

(defun reply-audio (audio title performer)
  (send-audio (get-current-chat)
              audio
              title
              performer))

(defparameter *master-chat* nil)
(defparameter *manager-group* nil)

(defun save-master-chat ()
  (save-json-file (merge-pathnames "master.json"
                                   (get-data-dir))
                  (to-json-a `(("master" . ,*master-chat*)))))

(defun load-master-chat ()
  (let ((file (merge-pathnames "master.json"
                               (get-data-dir))))
    (when (probe-file file)
      (setf *master-chat*
            (assoc-value (load-json-file file)
                         "master")))))

(defun get-master-chat ()
  *master-chat*)

(load-master-chat)

(defun save-manager-group ()
  (save-json-file (merge-pathnames "groups.json"
                                   (get-data-dir))
                  (to-json-a `(("groups" . ,*manager-group*)))))

(defun load-manager-group ()
  (let ((file (merge-pathnames "groups.json"
                               (get-data-dir))))
    (when (probe-file file)
      (setf *manager-group*
            (assoc-value (load-json-file file)
                         "groups")))))

(load-manager-group)

(defun get-manager-group ()
  *manager-group*)

(defcommand
    (:managergroup "添加此会话为机器人管理的会话" chat text)
    (declare (ignorable text))
    (let ((chat-id (cl-telegram-bot/chat:get-chat-id chat)))
      (if (find chat-id *manager-group* :test #'=)
          (reply "这个会话已经添加过了哟")
          (progn
            (setf *manager-group*
                (append *manager-group*
                        (list chat-id)))
            (save-manager-group)
            (reply (format nil "添加成功:~A" (get-manager-group)))))))

(defcommand
    (:groups "列出机器人管理的会话" chat text)
    (declare (ignorable text))
    (reply
     (format nil
             "会话列表：~%~{~A~}"
             *manager-group*)))

(in-package :cl-user)
