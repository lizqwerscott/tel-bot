(defpackage tel-bot.bot
  (:use :cl :cl-telegram-bot :cl-telegram-bot/message :tel-bot.head)
  (:export

   :create-bot
   :start-bot
   :stop-bot

   :defcommand

   :manager-bot
   :make-manager-bot

   :get-master-chat
   :get-manager-group

   :send-text))
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

;; (defmethod on-message ((bot manager-bot) text)
;;   )

;; Help command
(defparameter *command-infos* (make-hash-table :test #'eql))

(defmethod on-command ((bot manager-bot) (command (eql :help)) text)
  (declare (ignorable text))
  (let ((help-text nil))
    (maphash #'(lambda (command info)
                 (setq help-text
                       (append help-text
                               (list
                                (format nil "/~(~A~): ~A" command info)))))
             *command-infos*)
    (reply (format nil "命令介绍:~%~{~A~%~}" help-text))))

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
  (send-message *bot*
                (cl-telegram-bot/chat:get-chat-by-id *bot* chat-id)
                text))

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
                         "master")) )))

(defun get-master-chat ()
  *master-chat*)

(load-master-chat)

(defun save-manager-group ()
  (save-json-file (merge-pathnames "groups.json"
                                   (get-data-dir))
                  (to-json-a `(("groups" . ,*master-chat*)))))

(defun load-master-group ()
  (let ((file (merge-pathnames "groups.json"
                               (get-data-dir))))
    (when (probe-file file)
      (setf *master-chat*
            (assoc-value (load-json-file file)
                         "groups")) )))

(load-master-group)

(defun get-manager-group ()
  *manager-group*)

(defcommand
    (:managergroup "添加此会话为机器人管理的会话" chat text)
    (declare (ignorable text))
    (setf *manager-group*
        (append *manager-group*
                (list
                 (cl-telegram-bot/chat:get-chat-id
                  (get-current-chat)))))
    (reply (format nil "添加成功:~A" (get-manager-group))))

(in-package :cl-user)
