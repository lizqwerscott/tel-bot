(defpackage tel-bot.bot
  (:import-from :cl-telegram-bot/message
   :get-current-chat)
  (:import-from :cl-telegram-bot/chat
   :get-chat-id)
  (:import-from :cl-telegram-bot/chat
   :get-chat-by-id)
  (:import-from :tel-bot.chatgpt :ask)
  (:import-from :tel-bot.chatgpt :change-mode)
  (:import-from :tel-bot.chatgpt :get-mode)
  (:import-from :str :starts-with?)
  (:import-from :str :containsp)
  (:import-from :str :trim)
  (:import-from :alexandria :when-let)
  (:import-from :tel-bot.intent :predict-intent)
  (:use :cl :cl-telegram-bot :tel-bot.head :lzputils.json :lzputils.string :lzputils.used :easy-config)
  (:export

   :create-bot
   :start-bot
   :stop-bot

   :defcommand
   :add-command

   :add-reply-message

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
   (get-config "proxy"))

(defun create-bot ()
  (setf *bot*
        (make-manager-bot (get-config "bot-token")
                          :debug t)))

(defun start-bot ()
  (start-processing *bot*))

(defun stop-bot ()
  (stop-processing *bot*))

;; Help command
(defparameter *command-infos* (make-hash-table :test #'eql))
(defparameter *command-condition* (make-hash-table :test #'equal))

(defun help ()
  (let ((help-text nil))
    (maphash #'(lambda (command info)
                 (setq help-text
                       (append help-text
                               (list
                                (format nil "/~(~A~): ~A" command info)))))
             *command-infos*)
    (format nil "命令介绍:~%~{~A~%~}" help-text)))


;;; Last
;; (if (and (include-words? text
;;                        '("会" "懂" "能做"))
;;        (include-words? text
;;                        '("什么")))
;;     )

(defun is-group ()
  (typep (get-current-chat) 'cl-telegram-bot/chat::base-group))

(defun add-command (condition command)
  (multiple-value-bind (value is-find) (gethash condition
                                                *command-condition*)
    (setf (gethash condition
                   *command-condition*)
          (if is-find
              (append1 value
                       command)
              (list command)))))

;; (add-command "LAUNCH"
;;              '((("name" . "mc"))
;;                ("index")
;;                #'(lambda (index)
;;                    (format t "start mc ~A~%" index))))

(defun command-match (slots conditions &optional (matchp t))
  (if conditions
      (let ((condition (car conditions)))
        (command-match slots
                       (cdr conditions)
                       (and matchp
                          (when-let (value (assoc-value slots
                                                        (car condition)))
                            (containsp (cdr condition)
                                       value)))))
      matchp))

(defun commands-match-and-use (commands slots)
  (when commands
    (let ((command (car commands)))
      (if (command-match slots
                         (first command))
          (handler-case
              (apply (third command)
                     (mapcar #'(lambda (argv)
                                 (assoc-value slots argv))
                             (second command)))
            (error (c)
              (reply (format nil "[Error]: ~A" c))))
          (commands-match-and-use (cdr commands)
                                  slots)))))

(defun handle-command (text)
  (uiop:if-let (res (predict-intent text))
    (multiple-value-bind (value is-find) (gethash (assoc-value res "intent")
                                                  *command-condition*)
      (if is-find
          (commands-match-and-use value
                                  (assoc-value res
                                               "slots"))
          (log:info "not find intent: ~A~%" (assoc-value res "INTENT"))))
    (log:info "not predict intent: ~A~%" text)))

(defun handle-message (text)
  (when text
    (reply
     (if (include-words? text
                         '("help"))
         (help)
         (if (starts-with? "/" text)
             "命令错误"
             (when (not (handle-command text))
               (ask text)))))))

;; (defvar *test* '(text 嗯 reply_to_message
;;                  (text 微信消息{21549530346@chatroom}
;;                   [譕畏.]  in [骑猪找驴🥱🥱🥱] say:
;;                   我看好多人都不填
;;                   date 1679281124 chat
;;                   (type private username lizqwer last_name scott first_name lizqwer
;;                    id 1060310332)
;;                   from
;;                   (username kk_manage_bot first_name 初音未来 is_bot T id 5706957622)
;;                   message_id 2470)
;;                  date 1679281200 chat
;;                  (type private username lizqwer last_name scott first_name lizqwer id
;;                   1060310332)
;;                  from
;;                  (language_code zh-hans username lizqwer last_name scott first_name
;;                   lizqwer is_bot NIL id 1060310332)
;;                  message_id 2472))

(defvar *reply-message* nil)

(defun add-reply-message (fn)
  (setf *reply-message*
        (append1 *reply-message*
                 fn)))

(defmethod on-message ((bot manager-bot) text)
  (let ((raw-data (cl-telegram-bot/message:get-raw-data
                   cl-telegram-bot/message::*current-message*)))
    ;; (format t
    ;;         "raw-data: ~A~%"
    ;;         (third raw-data))
    (if (string= "reply_to_message" (third raw-data))
        (progn
          (format t "is reply~%")
          (dolist (i *reply-message*)
            (handler-case
                (apply i `(,text ,(fourth raw-data)))
              (error (c)
                (reply (format nil "Error: ~A~%" c))))))
        (let ((words (trim text)))
          (format t "message: ~A~%" words)
          (handle-message
           (trim
            (if (is-group)
                (when (start-with-words? words
                                         '("初音" "miku" "初音未来" "@kk_manage_bot"))
                  (replace-all-l '("初音" "miku" "初音未来" "@kk_manage_bot")
                                 ""
                                 text))
                text)))))))

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
               (get-config "bot-token")
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
                                   (get-data-path))
                  (to-json-a `(("master" . ,*master-chat*)))))

(defun load-master-chat ()
  (let ((file (merge-pathnames "master.json"
                               (get-data-path))))
    (when (probe-file file)
      (setf *master-chat*
            (assoc-value (load-json-file file)
                         "master")))))

(defun get-master-chat ()
  *master-chat*)

(load-master-chat)

(defun save-manager-group ()
  (save-json-file (merge-pathnames "groups.json"
                                   (get-data-path))
                  (to-json-a `(("groups" . ,*manager-group*)))))

(defun load-manager-group ()
  (let ((file (merge-pathnames "groups.json"
                               (get-data-path))))
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

(defcommand
    (:nowmode "获取现在机器人的模式" chat text)
    (declare (ignorable text))
    (reply
     (format nil
             "现在的模式是~A模式"
             (if (get-mode)
                 "记忆"
                 "普通"))))

(defcommand
    (:changemode "改变机器人对话的模式, 记忆模式下可以记住你以前说的话." chat text)
    (declare (ignorable text))
    (change-mode)
    (reply
     (format nil
             "现在的模式是~A模式"
             (if (get-mode)
                 "记忆"
                 "普通"))))

(in-package :cl-user)
