(defpackage tel-bot.chatgpt
  (:use :cl :tel-bot.head :tel-bot.web :lzputils.json :easy-config)
  (:export
   :ask))
(in-package :tel-bot.chatgpt)

(defun chat-gpt-ask (data api-key)
  (handler-case
      (multiple-value-bind (body status respone-headers uri stream)
          (dex:post "https://api.openai.com/v1/chat/completions"
                    :headers `(("Content-Type" . "application/json")
                               ("Authorization" . ,(format nil "Bearer ~A" api-key)))
                    :content (to-json-a
                              `(("model" . "gpt-3.5-turbo")
                                ("messages" . ((("role" . "user")
                                                ("content" . ,data)))))))
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
  (handle-message
   (chat-gpt-ask content
                 (get-config "chatgpt"))))

(in-package :cl-user)
