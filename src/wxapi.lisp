(defpackage tel-bot.wxapi
  (:import-from :str :trim)
  (:use :cl :cl-telegram-bot :tel-bot.bot :tel-bot.web :websocket-driver-client :lzputils.json :easy-config :lzputils.used)
  (:export
   :start-ws
   :stop-ws
   ))
(in-package :tel-bot.wxapi)

(defvar *client* (wsd:make-client "ws://10.0.96.67:5757"))

(defvar *id-user* nil)
(defvar *id-room* nil)
(defvar *group-list* nil)

(defparameter *group-link* (make-hash-table :test #'equal))

(defun json-to-hash-table (json)
  (dolist (kv json)
    (setf (gethash (car kv)
                   *group-link*)
          (cdr kv))))

(defun hash-table-to-json (table)
  (let ((res nil))
    (maphash #'(lambda (k v)
                 (setf res
                       (append1 res
                                `(,k . ,v))))
             table)
    res))

(defun save-group-link ()
  (save-json-file (merge-pathnames "group-links.json"
                                   (get-data-path))
                  (to-json-a `(("links" . ,(hash-table-to-json *group-link*))))))

(defun load-group-link ()
  (let ((file (merge-pathnames "group-links.json"
                               (get-data-path))))
    (when (probe-file file)
      (json-to-hash-table
       (assoc-value (load-json-file file)
                    "links")))))

(load-group-link)

(defun name-get-id (name)
  (and (find name *id-user* :test #'string= :key #'(lambda (x) (cdr x)))
     (find name *id-room* :test #'string= :key #'(lambda (x) (cdr x)))))

(defun generate-wx-message (data)
  (format nil
          "微信消息{~A}~%~A~%"
          (if (assoc-value data "roomp")
              (assoc-value data "room_id")
              (assoc-value data "sender_id"))
          (format nil
                  "[~A] ~A say:~%~A"
                  (assoc-value data "sender_name")
                  (if (assoc-value data "roomp")
                      (format nil " in [~A]" (assoc-value data "room_name"))
                      "")
                  (assoc-value data "content"))))

(wsd:on :message *client*
        (lambda (message)
          (let ((data (parse message)))
            (if (string= "recive_message" (assoc-value data "type"))
                (let ((wx-message (generate-wx-message data))
                      (group-name (gethash (assoc-value data "group") *group-link*)))
                  (if group-name
                      (send-text group-name
                                 wx-message)
                      (send-text (get-master-chat)
                                 wx-message)))
                (when (string= "user_list" (assoc-value data "type"))
                  (setf *id-user* (assoc-value data "user"))
                  (setf *id-room* (assoc-value data "room"))
                  (setf *group-list* (assoc-value data "groups")))))
          (format t "~A~%" message)))

(wsd:on :open *client*
        (lambda ()
          (format t "Connected~%")))

(defun remove-wrap (str)
  (subseq str 1 (- (length str) 1)))

(defun test ()
  (mapcar #'remove-wrap
          (cl-ppcre:all-matches-as-strings "(\\[).*?(\\])" "[hello] in [adasd]")))

(defun start-ws ()
  (wsd:start-connection *client*))

(defun stop-ws ()
  (wsd:close-connection *client*))

(defun text-get-id (text)
  (car
   (mapcar #'remove-wrap
           (cl-ppcre:all-matches-as-strings "{.*?}" text))))

(defun send-wx-message (id message)
  (wsd:send-text *client*
                 (to-json-a
                  `(("action" . "send_message")
                    ("content" . ,message)
                    ("wx_id" . ,id)))))

(add-reply-message
 #'(lambda (text reply-message)
     (let ((id (text-get-id (second reply-message))))
       (format t "send message:~A: ~A~%" id text)
       (send-wx-message id text))))

(defcommand
    (:linkgroup "添加此会话到微信分组" chat text)
    (let ((chat-id (cl-telegram-bot/chat:get-chat-id chat))
          (group-name (trim text)))
      (if (find group-name *group-list* :test #'string=)
          (progn
            (setf (gethash text *group-link*)
                  chat-id)
            (save-group-link)
            (reply
             (format nil "连接成功:~A to ~A" text chat-id)))
          (reply "没有找到这个分组!"))))

(defcommand
    (:grouplinks "列出分组对应的群" chat text)
    (declare (ignorable text))
    (reply
     (let ((res nil))
       (maphash #'(lambda (k v)
                    (setq res
                          (append1 res
                                   (format nil "~A: ~A" k v))))
                *group-link*)
       (let ((r-text (str:join "~%" res)))
         (if (string= "" r-text)
             "现在没有对应表"
             (format nil (format nil "以下是对应表:~%~A" r-text)))))))

(in-package :cl-user)
