(defpackage tel-bot.wxapi
  (:import-from :quri :make-uri)
  (:import-from :str :trim)
  (:use :cl :cl-telegram-bot :tel-bot.bot :tel-bot.web :websocket-driver-client :lzputils.json :easy-config :lzputils.used :tel-bot.head)
  (:export
   :start-ws
   :stop-ws
   ))
(in-package :tel-bot.wxapi)

(defvar *client* (wsd:make-client "ws://10.0.96.67:5757"))

(defvar *id-user* nil)
(defvar *id-room* nil)
(defvar *group-list* nil)

(defvar *group-link* (make-hash-table :test #'equal))
(defvar *last-say-message* (make-hash-table :test #'equal))

(defvar *last-message-id* (make-hash-table))

(defun json-to-hash-table (json)
  (dolist (kv json)
    (setf (gethash (car kv)
                   *group-link*)
          (cdr kv))
    (setf (gethash (car kv)
                   *last-say-message*)
          nil)))

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
  (car
   (or (find name *id-user* :test #'string= :key #'(lambda (x) (cdr x)))
      (find name *id-room* :test #'string= :key #'(lambda (x) (cdr x))))))

(defun xor (a b)
  (or (and a b)
     (and (not a)
        (not b))))

(defun get-content (data)
  (if (string= (assoc-value data "message_type") "picture")
      ""
      (assoc-value data "content")))

(defun generate-wx-message (data)
  (let ((roomp (assoc-value data "roomp"))
        (content (get-content data))
        (last-message (gethash (assoc-value data "group")
                               *last-say-message*)))
    (format nil
            (if (and last-message
                   (xor roomp
                        (assoc-value last-message
                                     "roomp")))
                (if (string= (assoc-value data "sender_id")
                             (assoc-value last-message "sender_id"))
                    (format nil
                            "~A"
                            content)
                    (if (string= (assoc-value data "room_id")
                                 (assoc-value last-message "room_id"))
                        (format nil
                                "[~A] say:~A~A"
                                (assoc-value data "sender_name")
                                (if (> (length content) 5)
                                    "~%"
                                    " ")
                                content)
                        (format nil
                                "[~A]~A say:~A~A"
                                (assoc-value data "sender_name")
                                (if roomp
                                    (format nil " in [~A]" (assoc-value data "room_name"))
                                    "")
                                (if (> (length content) 5)
                                    "~%"
                                    " ")
                                content)))
                (format nil
                        "[~A]~A say:~A~A"
                        (assoc-value data "sender_name")
                        (if roomp
                            (format nil " in [~A]" (assoc-value data "room_name"))
                            "")
                        (if (> (length content) 5)
                            "~%"
                            " ")
                        content)))))

(defun send-message-wx (type id content)
  (getf (if (string= type "text")
            (send-text id
                       content)
            (if (string= type "picture")
                (send-picture id
                              (first content)
                              (second content))))
        :|message_id|))

(defun download-picture (file-name &optional (path (ensure-directories-exist
                                                    (merge-pathnames "pictures/"
                                                                     (get-data-path)))))
  (download-url (make-uri :scheme "http"
                          :host "10.0.96.8"
                          :port 5556
                          :path "/picture/get"
                          :query `(("name" . ,file-name)))
                (merge-pathnames file-name
                                 path)))

(wsd:on :message *client*
        (lambda (message)
          (let ((data (parse message)))
            (if (string= "recive_message" (assoc-value data "type"))
                (let ((wx-message (generate-wx-message data))
                      (group-name (gethash (assoc-value data "group")
                                           *group-link*)))
                  (let ((message-id (send-message-wx (assoc-value data "message_type")
                                                     (if group-name
                                                         group-name
                                                         (get-master-chat))
                                                     (if (and (string= (assoc-value data "message_type") "picture")
                                                            (assoc-value data '("content" "havep")))
                                                         (list
                                                          (download-picture
                                                           (assoc-value data
                                                                        '("content" "pic" "name")))
                                                          wx-message)
                                                         wx-message))))
                    (setf (gethash message-id
                                   *last-message-id*)
                          (assoc-value data "wx_id")))
                  ;; 记录上次的消息
                  (setf (gethash (assoc-value data "group")
                                 *last-say-message*)
                        data))
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

;; (add-reply-message
;;  #'(lambda (text reply-message)
;;      (let ((id (text-get-id (second reply-message))))
;;        (format t "send message:~A: ~A~%" id text)
;;        (send-wx-message id text))))

(add-reply-message
 #'(lambda (text reply-id)
     (let ((id (gethash reply-id *last-message-id*)))
       (when id
         (format t "send message:~A: ~A~%" id text)
         (send-wx-message id text)))))


(maphash #'(lambda (k v)
             (add-special-group-handle v
                                       #'(lambda (text)
                                           (let ((last-message (gethash k *last-say-message*)))
                                             (when last-message
                                               (send-wx-message (assoc-value last-message "wx_id")
                                                                text))))))
         *group-link*)

(defcommand
    (:linkgroup "添加此会话到微信分组" chat text)
    (let ((chat-id (cl-telegram-bot/chat:get-chat-id chat))
          (group-name (trim text)))
      (if (find group-name *group-list* :test #'string=)
          (progn
            (setf (gethash text *group-link*)
                  chat-id)
            (setf (gethash text *last-say-message*)
                  "")
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

(defcommand
    (:sendwx "手动发送消息到指定的群或者人" chat text)
    (let ((data (str:split " " text)))
      (if (> (length data) 1)
          (let ((id (name-get-id (car data))))
            (if id
                (progn
                  (send-wx-message id (str:join " " (cdr data)))
                  (reply
                   (format nil "已经发送给: ~A" id)))
                (reply "没有找到这个人或者群")))
          (reply "参数不够"))))

(in-package :cl-user)
