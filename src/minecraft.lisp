(defpackage :tel-bot.minecraft
  (:import-from :alexandria :switch)
  (:import-from :alexandria :iota)
  (:import-from :str :join)
  (:use :cl :tel-bot.head :tel-bot.bot :tel-bot.web :cl-telegram-bot :lzputils.json :easy-config :lzputils.used))
(in-package :tel-bot.minecraft)

(defvar *key* (get-config "mc-token"))
(defvar *address* (get-config "mc-address"))

(defparameter *instances* nil)

(defun instance-status (instance)
  (switch ((elt instance 2) :test #'=)
    (-1 "状态未知")
    (0 "已停止")
    (1 "正在停止")
    (2 "正在启动")
    (3 "正在运行")))

(defun instance-player (instance)
  (if (stringp (elt instance 3))
      (parse-integer (elt instance 3))
      0))

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
                                (instance-status instance)
                                (instance-player instance)))
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

(defun manager-instance-command (command uuid remote-uuid &optional (other-args nil))
  (handle-data
   (handler-case
       (web-get *address*
                (format nil "api/protected_instance/~A" command)
                :args (append `(("apikey" . ,*key*)
                                ("remote_uuid" . ,remote-uuid)
                                ("uuid" . ,uuid))
                              other-args)
                :jsonp t)
     (dex:http-request-failed (e)
       (yason:parse (dex:response-body e))))))

(defun manager-instances-command1 (command instance)
  (manager-instance-command command
                            (second (second instance))
                            (first instance)))

(defun manager-instance (command instance zh-command good)
  (progn
    (reply-text
     (format nil
             "正在~A实例......"
             zh-command))
    (let ((res (manager-instances-command1 command instance)))
      (reply
       (if (listp res)
           good
           (format nil "实例~A失败: ~A" zh-command res))))))

(defun start-instance (instance)
  (manager-instance "open" instance "启动" "实例需要几分钟才能启动完成，请等待几分钟后在进入"))

(defun stop-instance (instance)
  (manager-instance "stop" instance "关闭" "实例关闭成功"))

(defun restart-instance (instance)
  (manager-instance "restart" instance "重启" "实例需要几分钟才能重启成功，请等待几分钟后在进入"))

(defun kill-instance (instance)
  (manager-instance "kill" instance "强制关闭" "实例强制关闭成功"))

(defun instance-send-command (command instance)
  (manager-instance-command "command"
                            (second (second instance))
                            (first instance)
                            `(("command" . ,command))))

(defun instance-get-log (uuid remote-uuid)
  (manager-instance-command "outputlog"
                            uuid
                            remote-uuid))

(defun test ()
  (let ((instance (search-instance 1)))
    (instance-send-command "/say 👋"
                           instance)))

;; (defun test-1 (&optional (n 10))
;;   (let ((instance (search-instance 1)))
;;     (let ((lines (str:lines (instance-get-log (second (second instance)) (car instance)))))
;;       (dotimes (i n)
;;         (format t "~A~%" (elt lines (- (length lines) i 1)))))
;;     (instance-get-log (second (second instance)) (car instance))))
;; (let ((strs (instance-get-log (second (second instance)) (car instance))))
;;   (with-open-file (out "~/a.txt" :direction :output :if-does-not-exist :create)
;;     (write-sequence strs out)))

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

(defun parse-text-instance (text)
  (if-return (search-instance (parse-integer text))
    (reply "没有发现这个实例")))

(defcommand
    (:mcstart "启动指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
        (start-instance (parse-text-instance text)))
    (error (c)
           (reply (format nil "[Error]: ~A" c))))

(defcommand
    (:mcstop "关闭指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
        (stop-instance (parse-text-instance text)))
    (error (c)
           (reply (format nil "[Error]: ~A" c))))

(defcommand
    (:mcrestart "重启指定序号的mc服务器" chat text)
    (declare (ignorable text))
    (refersh-instaces)
    (handler-case
        (restart-instance (parse-text-instance text))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:mckill "强制关闭指定序号的mc服务器" chat text)
    (refersh-instaces)
    (handler-case
        (kill-instance (parse-text-instance text))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:mcaction "向指定序号的mc服务器发送指令, 例: /mcaction 1 /say hello" chat text)
  (refersh-instaces)
  (handler-case
      (let ((temp (split-s text)))
        (let ((index (parse-integer (car temp)))
              (command (join " " (cdr temp))))
          (let ((instance (search-instance index)))
            (if instance
                (progn
                  (reply-text (format nil
                                      "正在执行命令: ~A"
                                      command))
                  (instance-send-command command instance)
                  (reply "命令执行完毕"))
                (reply "没有发现这个实例")))
          (format t "command: ~A~%" temp)))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
