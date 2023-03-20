(defpackage tel-bot.wxapi
  (:use :cl :tel-bot.bot :tel-bot.web :websocket-driver-client :lzputils.json)
  (:export
   :start-ws
   :stop-ws
   ))
(in-package :tel-bot.wxapi)

(defvar *client* (wsd:make-client "ws://10.0.96.67:5757"))

(defvar *id-user* nil)
(defvar *id-room* nil)

(defun name-get-id (name)
  (and (find name *id-user* :test #'string= :key #'(lambda (x) (cdr x)))
     (find name *id-room* :test #'string= :key #'(lambda (x) (cdr x)))))

(wsd:on :message *client*
        (lambda (message)
          (let ((data (parse message)))
            (if (string= "recive_message" (assoc-value data "type"))
                (send-text (get-master-chat)
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
                (when (string= "user_list" (assoc-value data "type"))
                  (setf *id-user* (assoc-value data "user"))
                  (setf *id-room* (assoc-value data "room")))))
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

(in-package :cl-user)
