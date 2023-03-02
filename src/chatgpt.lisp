(defpackage tel-bot.chatgpt
  (:use :cl :tel-bot.head :tel-bot.web :lzputils.json :easy-config :lzputils.used)
  (:export
   :get-mode
   :change-mode
   :ask))
(in-package :tel-bot.chatgpt)

;; (defvar *system-command* "你现在是初音未来, 现在作为我的老婆和我对话.")
(defvar *system-command* "你现在是猫耳女仆")
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
                                ("max_tokens" . 500)
                                ("messages" . ,*last-messages*))))
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
      (assoc-value (car messages)
                   (list "message" "content")))))

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
    message))

(in-package :cl-user)
