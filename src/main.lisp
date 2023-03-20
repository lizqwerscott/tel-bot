(defpackage tel-bot
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :lzputils.json :assoc-value)
  (:use
   :cl
   :tel-bot.head
   :tel-bot.bot
   :tel-bot.task
   :tel-bot.text
   :tel-bot.picture
   :tel-bot.wxapi)
  (:export
   :start
   :stop))
(in-package :tel-bot)

(create-bot)

(add-task #'(lambda ()
              (mapcar #'(lambda (chat-id)
                          (send-text chat-id
                                     (format nil
                                             "大家晚安哟!~%~%~A"
                                             (wanan))))
                      (get-manager-group))
              (when (get-master-chat)
                (send-text (get-master-chat)
                           (format nil
                                   "主人晚安!~%~%~A"
                                   (wanan)))))
          "goodnight"
          (list 23 00 :step-mintue 20))
(start-task "goodnight")

(defun zaoan-f ()
  (let ((h (holidays))
        (line (english-line)))
    (format nil
            "早安，~A!~%今天是~A ~A ~A~%~A~%每日一句:~%~A~%~A~%~%~A"
            (today-format t)
            (assoc-value h "lunaryear")
            (assoc-value h "lunarmonth")
            (assoc-value h "lunarday")
            (assoc-value h "name")
            (assoc-value line "content")
            (assoc-value line "note")
            (zaoan))))

(defun zaoan-fun ()
  (let ((text (zaoan-f)))
    (mapcar #'(lambda (chat-id)
                (send-text chat-id
                           text)
                (send-picture chat-id
                              (random-picture)))
            (get-manager-group))))

(add-task #'(lambda ()
              (zaoan-fun)
              (when (get-master-chat)
                (send-text (get-master-chat)
                           (format nil
                                   "主人早安哟!~%~%~A"
                                   (zaoan)))))
          "goodmorning"
          (list 8 0 :step-mintue 20))
(start-task "goodmorning")

(defun run ()
  (format t "Start patron...~%")
  (start-patron)
  (do ((i 0 (+ i 1)))
      (nil i)
    ;; reset all day task
    (when (and (apply #'time-in (get-time-range 0 0))
               (not (is-reset)))
      (format t "reset all task run~%")
      (reset-task-time))
    ;; run day task
    (dolist (task (run-tasks))
      (when (and (not (task-runp task))
                 (apply #'time-in
                        (apply #'get-time-range
                               (task-time task))))
        (create-job (task-func task))
        (setf (task-runp task) t)))
    (sleep 1))
  (format t "Stop patron...~%")
  (stop-patron))

(defun start (&optional (is-debug nil))
  (start-bot)
  (start-ws)
  (if is-debug
      (make-thread #'run)
      (run)))

(defun stop ()
  (stop-bot)
  (stop-ws))
