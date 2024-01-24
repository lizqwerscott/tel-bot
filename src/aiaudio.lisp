(defpackage :tel-bot.aiaudio
  (:import-from :lzputils.json :assoc-value)
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :local-time :cl-telegram-bot :easy-config)
  (:export
   ))
(in-package :tel-bot.aiaudio)

(defvar *ai-address* "127.0.0.1:5000")

(defun web-audio-load-model ()
  (handler-case
      (web-get *ai-address*
               "models/add"
               :args `(("model_path" . "./Data/Taffy/models/G_11100.pth")
                       ("config_path" . "./Data/Taffy/config.json"))
               :jsonp nil
               :read-timeout 20)
    (error (c)
      (log:error "load model error: ~A" c))))

(defun web-audio-handle (speaker-text &key (emotion "Happy") (style-text nil) (language "ZH") (speaker-name "永雏塔菲"))
  (handler-case
      (web-get *ai-address*
               "voice"
               :args `(("text" . ,speaker-text)
                       ("model_id" . 0)
                       ("speaker_name" . ,speaker-name)
                       ("noise" . 0.8)
                       ("emotion" . ,emotion)
                       ("language" . ,language)
                       ("auto_split" . "true")
                       ("style_text" . ,style-text))
               :jsonp nil
               :read-timeout 20)
    (error (c)
      (log:error "get-audio with speaker-text: ~A error: ~A" speaker-text c))))

(defun get-audio (text)
  (let ((data (web-audio-handle text))
        (finish-path (make-file (format nil "taff-~A" (timestamp-to-unix (now)))
                                "wav"
                                (ensure-directories-exist
                                 (merge-pathnames "audio/"
                                                  (get-data-path))))))
    (when data
      (with-open-file (stream finish-path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence data stream))
      finish-path)))

(defun test ()
  (get-audio "大家好呀喵"))

(web-audio-load-model)

(defcommand
    (:say "永雏塔菲(测试)" chat text)
    (handler-case
        (let ((say-text (split-s (str:trim text))))
          (let ((res (get-audio say-text)))
            (format t "songs: ~A~%" res)
            (log:info "taff say: ~A, path: ~A~%" say-text res)
            (if res
                (reply-voice res)
                (reply "塔菲不能说话了喵!!!"))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
