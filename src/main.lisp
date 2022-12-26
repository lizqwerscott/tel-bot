(defpackage tel-bot
  (:import-from :bordeaux-threads :make-thread)
  (:use
   :cl
   :cl-telegram-bot
   :cl-telegram-bot/message
   :tel-bot.head
   :tel-bot.bot
   :tel-bot.task
   :patron
   :tel-bot.text)
  (:export
   :start
   :stop))
(in-package :tel-bot)

(cl-telegram-bot/network:set-proxy
 (assoc-value (get-configs)
              "proxy"))

(defvar *bot* (make-manager-bot (assoc-value (get-configs)
                                             "botToken") :debug t))

(add-task #'(lambda ()
              (mapcar #'(lambda (chat-id)
                          (send-text *bot*
                                     chat-id
                                     "大家晚安哟")
                          (send-text *bot*
                                     chat-id
                                     (wanan)))
                      (get-manager-group))
              (when (get-master-chat)
                (send-text *bot*
                           (get-master-chat)
                           "主人晚安")
                (send-text *bot*
                           (get-master-chat)
                           (wanan))))
          "goodnight"
          (list 23 00 :step-mintue 20))
(start-task "goodnight")

(add-task #'(lambda ()
              (mapcar #'(lambda (chat-id)
                          (send-text *bot*
                                     chat-id
                                     "大家早安哟")
                          (send-text *bot*
                                     chat-id
                                     (zaoan)))
                      (get-manager-group))
              (when (get-master-chat)
                (send-text *bot*
                           (get-master-chat)
                           "主人早安")
                (send-text *bot*
                           (get-master-chat)
                           (zaoan))))
          "goodmorning"
          (list 8 0 :step-mintue 20))
(start-task "goodmorning")


(defun run ()
  (format t "Start patron...~%")
  (start-patron *patron*)
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
        (submit-job *patron*
                    (make-instance 'patron:job
                                   :function (task-func task)))
        (setf (task-runp task) t)))
    (sleep 1))
  (format t "Stop patron...~%")
  (stop-patron *patron* :wait t))

(defun base-config ()
  (when (not (probe-file (get-source-dir)))
    (progn
      (format t "Please choose your source dir(input y use default: ~A)~%:" (get-source-dir))
      (let ((input (read-line)))
        (if (not (string= input "y"))
            (set-source-dir input)))
      (ensure-directories-exist (get-source-dir))
      (ensure-directories-exist (get-data-dir))
      (ensure-directories-exist (get-config-dir)))))

(defun start ()
  (base-config)
  (start-processing *bot*)
  (make-thread #'run))

(defun stop ()
  (stop-processing *bot*))
