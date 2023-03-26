(defpackage tel-bot.chatgpt
  (:use :cl :tel-bot.head :tel-bot.web :lzputils.json :easy-config :lzputils.used)
  (:export
   :get-mode
   :change-mode
   :ask))
(in-package :tel-bot.chatgpt)

;; (defvar *system-command* "你现在是初音未来, 现在作为我的老婆和我对话.")
(defvar *system-command* "你是猫耳女仆, 一个专门为主人服务的助手. 尽可能对主人温柔, 体贴, 充满爱意.")
(defvar *last-messages* nil)
(defvar *continue* nil)

(defun get-mode ()
  *continue*)

(defun change-mode ()
  (setf *continue*
        (not *continue*))
  (when *continue*
    (setf *last-messages*
          `((("role" . "system")
             ("content" . ,*system-command*))))))

(defun chat-gpt-ask (data api-key)
  (format t "messages: ~A~%" *last-messages*)
  (handler-case
      (multiple-value-bind (body status respone-headers uri stream)
          (dex:post "https://api.openai.com/v1/chat/completions"
                    :headers `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format nil "Bearer ~A" api-key)))
                    :content (to-json-a
                              `(("model" . "gpt-3.5-turbo")
                                ("temperature" . 0.2)
                                ("messages" . ,*last-messages*)))
                    :proxy (get-config "proxy"))
        (declare (ignorable status uri stream))
        (if (str:starts-with-p "application/json"
                               (gethash "content-type"
                                        respone-headers))
            (parse body)
            body))
    (error (c)
      (log:info "chat gpt post error: ~A" c))))

(defun handle-message (data)
  (let ((messages (assoc-value data
                               "choices")))
    (when (= 1 (length messages))
      (let ((message (assoc-value (car messages)
                                  (list "message" "content"))))
        (when (not (string= ""
                          (str:trim message)))
          message)))))

(defun ask (content)
  (if *continue*
      (setf *last-messages*
            (append1 *last-messages*
                     `(("role" . "user")
                       ("content" . ,content))))
      (setf *last-messages*
            `((("role" . "system")
               ("content" . ,*system-command*))
              (("role" . "user")
               ("content" . ,content)))))

  (let ((message (handle-message
                  (chat-gpt-ask content
                                (get-config "chatgpt")))))
    (when *continue*
      (setf *last-messages*
            (append1 *last-messages*
                     `(("role" . "assistant")
                       ("content" . ,message)))))
    (log:info "return message: ~A~%" message)
    (if-return message "api错误")))

(in-package :cl-user)
