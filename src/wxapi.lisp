(defpackage tel-bot.wxapi
  (:use :cl :tel-bot.bot :tel-bot.web :websocket-driver-client :lzputils.json)
  (:export
   ))
(in-package :tel-bot.wxapi)

(defvar *client* (wsd:make-client "ws://127.0.0.1:5757"))

(defvar *id-user* nil)
(defvar *id-room* nil)

(defun name-get-id (name)
  (and (find name *id-user* :test #'string= :key #'(lambda (x) (cdr x)))
     (find name *id-room* :test #'string= :key #'(lambda (x) (cdr x)))))

(wsd:start-connection *client*)
(wsd:on :message *client*
        (lambda (message)
          (let ((data (parse message)))
            (if (string= "recive_message" (assoc-value data "type"))
                (send-text (get-master-chat)
                           (format nil
                                   "微信消息~%~A~%"
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

(in-package :cl-user)
