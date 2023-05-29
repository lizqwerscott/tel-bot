(defpackage tel-bot.wxhttp
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :quri :make-uri)
  (:import-from :alexandria :when-let)
  (:import-from :str :trim)
  (:import-from :mk-string-metrics :levenshtein)
  (:use
   :cl
   :cl-telegram-bot
   :tel-bot.bot
   :tel-bot.web
   :lzputils.json
   :lzputils.string
   :easy-config
   :lzputils.used
   :tel-bot.head)
  (:export
   :start-wx
   :stop-wx
   :restart-wx))
(in-package :tel-bot.wxhttp)

(defparameter *host* "10.0.96.37")
(defparameter *http-port* 5757)
(defparameter *picture-port* 5556)

(defparameter *server-run* nil)

(defun get-http-url ()
  (format nil "~A:~A" *host* *http-port*))

(defun get-picture-url ()
  (format nil "~A:~A" *host* *picture-port*))

(defun handle-data (data)
  (when data
    (if (= (assoc-value data "code"))
        (assoc-value data "data")
        (log:error "Http error: ~A~%"
                   (assoc-value data "msg")))))

(defun get-userlist ()
  (handle-data
   (web-get (get-http-url)
            "user/userlist"
            :jsonp t)))

(defun get-updates ()
  (handle-data
   (web-get (get-http-url)
            "message/getUpdates"
            :jsonp t)))

(defun send-wx-text (id text)
  (handle-data
   (web-post (make-uri :scheme "http"
                       :host *host*
                       :port *http-port*
                       :path "/message/sendText")
             :args `(("wx_id" . ,id)
                     ("content" . ,text))
             :jsonp t)))

(defun send-wx-picture (id pic-url)
  (handle-data
   (web-post (make-uri :scheme "http"
                       :host *host*
                       :port *http-port*
                       :path "/message/sendPicture")
             :args `(("wx_id" . ,id)
                     ("url" . ,pic-url))
             :jsonp t)))

(defvar *id-user* nil)
(defvar *id-room* nil)
(defvar *group-list* nil)

(defvar *group-link* (make-hash-table :test #'equal))
(defvar *last-say-message* (make-hash-table :test #'equal))
(defvar *reply-p* nil)

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
  (append
   (remove-if-not #'(lambda (x)
                      (str:contains? name (cdr x)))
                  *id-user*)
   (remove-if-not #'(lambda (x)
                      (str:contains? name (cdr x)))
                  *id-room*)))

(defun xor (a b)
  (or (and a b)
     (and (not a)
        (not b))))

(defun get-content (data)
  (if (string= (assoc-value data "message_type") "picture")
      ""
      (let ((content (assoc-value data "content")))
        (if (str:starts-with? "/html"
                              content)
            (str:replace-first "/html"
                               ""
                               content)
            (replace-all-l (list "&")
                           "&amp;"
                           (replace-all-l (list ">")
                                          "&gt;"
                                          (replace-all-l (list "<")
                                                         "&lt;"
                                                         content)))))))

(defun send-message-wx (type id content)
  (handler-case
      (getf (if (string= type "text")
                (send-html id
                           content)
                (if (string= type "picture")
                    (send-picture id
                                  (first content)
                                  (second content))))
            :|message_id|)
    (error (c)
      (log:error "send telegram message error:~A" content))))

(defun download-picture (file-name &optional (path (ensure-directories-exist
                                                    (merge-pathnames "pictures/"
                                                                     (get-data-path)))))
  (download-url (make-uri :scheme "http"
                          :host "192.168.2.198"
                          :port 5556
                          :path "/picture/get"
                          :query `(("name" . ,file-name)))
                (merge-pathnames file-name
                                 path)))

(defun same-personp (data last-data)
  (string= (assoc-value data "sender_id")
           (assoc-value last-data "sender_id")))

(defun same-roomp (data last-data)
  (string= (assoc-value data "room_id")
           (assoc-value last-data "room_id")))

(defun same-channelp (data last-data)
  (if (xor (assoc-value data "roomp")
           (assoc-value last-data
                        "roomp"))
      (if (assoc-value data "roomp")
          (same-roomp data last-data)
          (same-personp data last-data))))

(defun same-messagep (data last-data)
  (and last-data
     (not (string= (assoc-value data "message_type") "picture"))
     (and (same-channelp data last-data)
        (string= (get-content data)
                 (get-content last-data)))))

(defun calc-message-length (sender-name content show-roomp room-name)
  (+ (length sender-name)
     2
     (if show-roomp
         (+ 5
            (length
             room-name))
         0)
     (length content)))

(defun generate-wx-message (data)
  (let ((roomp (assoc-value data "roomp"))
        (content (get-content data))
        (last-message (gethash (assoc-value data "group")
                               *last-say-message*)))
    (if (and (same-channelp data last-message)
           (same-personp data last-message)
           (not *reply-p*))
        (format nil
                content)
        (let ((show-room-namep (and roomp
                                  (not
                                   (same-roomp data last-message)))))
          (format nil
                  "<b>~A</b>~A:~A~A"
                  (assoc-value data "sender_name")
                  (if show-room-namep
                      (format nil " in #~A" (assoc-value data "room_name"))
                      "")
                  (if (>= (calc-message-length (assoc-value data "sender_name")
                                              content
                                              show-room-namep
                                              (assoc-value data "room_name"))
                         22)
                      (format nil "~%")
                      " ")
                  content)))))

(defun handle-wx-message (data)
  ;;不发送自己在群里面发的图片
  (when (not (and (string= "self" (assoc-value data "sender_name"))
              (and (string= (assoc-value data "message_type") "picture")
                 (assoc-value data '("content" "havep")))))
    (let ((wx-message (generate-wx-message data))
          (group-name (gethash (assoc-value data "group")
                               *group-link*)))
      (if (not (same-messagep data
                            (gethash (assoc-value data "group")
                                     *last-say-message*)))
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
            (when message-id
              ;; 重置回复值
              (setf *reply-p* nil)
              (setf (gethash message-id
                             *last-message-id*)
                    (assoc-value data "wx_id"))
              ;; 记录上次的消息
              (setf (gethash (assoc-value data "group")
                             *last-say-message*)
                    data)))
          (log:info "message is same: ~A, ~A"
                    data
                    (gethash (assoc-value data "group")
                             *last-say-message*)))))
  (log:info "recive message: ~A" data))

(defun refresh-user-list ()
  (let ((user-list (get-userlist)))
    (setf *id-user* (assoc-value user-list "users"))
    (setf *id-room* (assoc-value user-list "rooms"))
    (setf *group-list* (assoc-value user-list "groups"))
    (log:info "refresh recive user list: ~A" user-list)))

(defun server-run ()
  (refresh-user-list)
  (do ((wait-time 1))
      ((not *server-run*) 'done)
    (handler-case
        (let ((res (get-updates)))
          (when (assoc-value res "update_list")
            (refresh-user-list))
          (if (assoc-value res "update")
              (progn
                (setf wait-time 1)
                (dolist (message (assoc-value res "messages"))
                  (handler-case
                      (handle-wx-message message)
                    (error (c)
                      (log:error "handle wx message error: ~A~%" c)))))
              (setf wait-time
                    (mod (+ 1 wait-time)
                         5))))
      (error (c)
        (log:error "Get updates errors: ~A" c)))
    (sleep wait-time)))

(defun start-wx ()
  (setf *server-run* t)
  (make-thread #'server-run :name "wxhttp"))

(defun stop-wx ()
  (setf *server-run* nil))

(defun restart-wx ()
  (stop-wx)
  (sleep 1)
  (start-wx))

(defun reply-message (id message)
  (setf *reply-p* t)
  (if (find (car message) '("text" "entities") :test #'string=)
      (send-wx-text id (cdr message))
      (if (find (car message) '("photo" "sticker") :test #'string=)
          (send-wx-picture id (cdr message)))))

(add-reply-message
 #'(lambda (message reply-id)
     (alexandria:when-let (id (gethash reply-id *last-message-id*))
       (format t "send message:~A: ~A~%" id message)
       (reply-message id message))))

(maphash #'(lambda (k v)
             (add-special-group-handle v
                                       #'(lambda (message)
                                           (alexandria:when-let (last-message (gethash k *last-say-message*))
                                             (reply-message (assoc-value last-message "wx_id")
                                                            message)))))
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
    (:chat "手动发送消息到指定的群或者人" chat text)
    (let ((data (str:split " " text)))
      (if (> (length data) 1)
          (let ((ids (name-get-id (car data))))
            (if (and ids
                   (< (length ids) 2))
                (progn
                  (send-wx-text (car (car ids)) (str:join " " (cdr data)))
                  (reply
                   (format nil "已经发送给: ~A" (car (car ids)))))
                (reply "没有找到这个人或者群")))
          (reply "参数不够"))))

(in-package :cl-user)
