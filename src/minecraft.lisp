(defpackage :tel-bot.minecraft
  (:import-from :alexandria :switch)
  (:import-from :alexandria :iota)
  (:import-from :str :join)
  (:use :cl :tel-bot.head :tel-bot.bot :tel-bot.web :cl-telegram-bot))
(in-package :tel-bot.minecraft)

(defvar *key* (assoc-value (get-configs)
                           "mcToken"))
(defvar *address* (assoc-value (get-configs)
                               "mcAddress"))

(defparameter *instances* nil)

(defun instance-status (status)
  (switch (status :test #'=)
    (-1 "状态未知")
    (0 "已停止")
    (1 "正在停止")
    (2 "正在启动")
    (3 "正在运行")))

(defun handle-data (data)
  (if (= 200 (assoc-value data "status"))
      (assoc-value data "data")
      (progn
        (format t "[Error]:~A~%" data)
        (assoc-value data "data"))))

(defun manager-status-simplified ()
  (handle-data
   (web-get *address*
            "api/service/remote_services_system"
            :args `(("apikey" . ,*key*))
            :jsonp t)))

(defun manager-status ()
  (let ((data (car (manager-status-simplified))))
    (format nil
            "主要信息:~%CPU使用: ~,2f%, 内存使用: ~,2f%~%运行中实例: ~d, 实例总个数: ~d~%"
            (* (assoc-value data '("system" "memUsage"))
               100)
            (* (assoc-value data '("system" "cpuUsage"))
               100)
            (assoc-value data '("instance" "running"))
            (assoc-value data '("instance" "total")))))

(defun manager-instances ()
  (let ((data (handle-data
               (web-get *address*
                        "api/service/remote_services"
                        :args `(("apikey" . ,*key*))
                        :jsonp t))))
    (mapcar #'(lambda (host)
                (list (assoc-value host "uuid")
                      (mapcar #'(lambda (instance)
                                  (assoc-value-l instance
                                                 (list
                                                  '("config" "nickname")
                                                  "instanceUuid"
                                                  "status"
                                                  '("info" "currentPlayers"))))
                              (assoc-value host "instances"))))
            data)))

(defun refersh-instaces ()
  (setq *instances*
        (manager-instances)))

(defun handle-instaces-info ()
  (str:join "~%"
            (mapcar #'(lambda (instance index)
                        (format nil
                                "实例(~A)~d: ~A, ~d个玩家"
                                (car instance)
                                index
                                (instance-status
                                 (elt instance 2))
                                (if (stringp (elt instance 3))
                                    (elt instance 3)
                                    0)))
                    (car (cdr (car *instances*)))
                    (iota (length (car (cdr (car *instances*))))
                          :start 1))))

(defun search-instance (index)
  (handler-case
      (list (car (car *instances*))
            (elt (car (cdr (car *instances*)))
                 (- index 1)))
    (error (c)
      (format t "Not have ~A instance" index))))

(defun manager-instance-command (command uuid remote-uuid)
  (handle-data
   (handler-case
       (web-get *address*
                (format nil "api/protected_instance/~A" command)
                :args `(("apikey" . ,*key*)
                        ("remote_uuid" . ,remote-uuid)
                        ("uuid" . ,uuid))
                :jsonp t)
     (dex:http-request-failed (e)
       (yason:parse (dex:response-body e))))))

(defun test ()
  (manager-instance-command "open" "3bcba904a6974cdf84eeba6c3e8f14d5" "6ffeaca6c142404299e46c43b2054d41"))

(defcommand
    (:mcstate "输出mc服务器状态" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (let ((status (manager-status))
        (instances (handle-instaces-info)))
    (reply
     (format nil
             "~A~A"
             status
             instances))))

(defcommand
    (:mcstart "启动指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
      (let ((index (parse-integer text)))
        (let ((instance (search-instance index)))
          (if instance
              (progn
                (send-text chat "正在启动实例......")
                (let ((res (manager-instance-command "open" (second (second instance)) (first instance))))
                  (reply
                   (if (listp res)
                       "实例需要几分钟才能启动完成，请等待几分钟后在进入"
                       (format nil "实例启动失败: ~A" res)))))
              (reply "没有发现这个实例"))))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:mcstop "关闭指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
      (let ((index (parse-integer text)))
        (let ((instance (search-instance index)))
          (if instance
              (progn
                (send-text chat "正在关闭实例......")
                (let ((res (manager-instance-command "stop" (second (second instance)) (first instance))))
                  (reply
                   (if (listp res)
                       "实例关闭成功"
                       (format nil "实例关闭失败: ~A" res)))))
              (reply "没有发现这个实例"))))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:mcrestart "重启指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
        (let ((index (parse-integer text)))
          (let ((instance (search-instance index)))
            (if instance
                (progn
                  (send-text chat "正在重新启动实例......")
                  (let ((res (manager-instance-command "restart" (second (second instance)) (first instance))))
                    (reply
                     (if (listp res)
                         "实例需要几分钟才能重启成功，请等待几分钟后在进入"
                         (format nil "实例重启失败: ~A" res)))))
                (reply "没有发现这个实例"))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:mckill "强制关闭指定序号的mc服务器" chat text)
    (refersh-instaces)
    (handler-case
        (let ((index (parse-integer text)))
          (let ((instance (search-instance index)))
            (if instance
                (progn
                  (send-text chat "正在强制关闭实例......")
                  (let ((res (manager-instance-command "kill" (second (second instance)) (first instance))))
                    (reply
                     (if (listp res)
                         "实例强制关闭成功"
                         (format nil "实例强制关闭失败: ~A" res)))))
                (reply "没有发现这个实例"))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
