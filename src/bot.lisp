(defpackage tel-bot.bot
  (:use :cl :cl-telegram-bot :cl-telegram-bot/message :tel-bot.head)
  (:export
   :manager-bot
   :make-manager-bot

   :get-master-chat
   :get-manager-group
   :send-text))
(in-package :tel-bot.bot)

(defbot manager-bot)

(defmethod on-message ((bot manager-bot) text)
  ;; (reply text)
  )

(defmethod on-command ((bot manager-bot) (command (eql :help)) text)
  (declare (ignorable text))
  (reply "Just send me any text and I'll reply with the same text."))

(defmethod on-command ((bot manager-bot) (command (eql :start)) text)
  (declare (ignorable text))
  (reply "Welcome Lisper"))

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

(defmethod on-command ((bot manager-bot) (command (eql :managergroup)) text)
  (declare (ignorable text))
  (setf *manager-group*
        (append *manager-group*
                (list
                 (cl-telegram-bot/chat:get-chat-id
                  (get-current-chat)))))
  (reply (format nil "添加成功:~A" (get-manager-group))))

(defun send-text (bot chat-id text)
  (send-message bot
                (cl-telegram-bot/chat:get-chat-by-id chat-id)
                text))

(in-package :cl-user)
