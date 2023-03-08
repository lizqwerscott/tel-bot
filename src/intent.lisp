(defpackage tel-bot.intent
  (:import-from :str :containsp)
  (:use :cl :lzputils.json :tel-bot.web :easy-config :lzputils.used)
  (:export
   :predict-intent
   :chinese-index-to-number
   ))
(in-package :tel-bot.intent)

(defun intent-post (text)
  (handler-case
      (multiple-value-bind (body status respone-headers uri stream)
          (dexador:post (format nil
                                "~A/text"
                                (get-config "intent-address"))
                        :content (to-json-a `(("text" . ,text)))
                        :headers '(("Content-Type" . "application/json; charset=utf-8")))
        (declare (ignorable status uri stream))
        (if (and (str:starts-with-p "application/json"
                                  (gethash "content-type"
                                           respone-headers)))
            (parse body)
            body))
    (error (c)
      (log:error "predict post error ~A" c))))

(defun handle-data (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "data")
      (progn
        (log:error "predict code: ~A, msg: ~A" (assoc-value data "code") (assoc-value data "msg"))
        nil)))

(defvar *number-chinese* (list "一" "二" "三" "四" "五" "六" "七" "八" "九" "十"))

(defun chinese-index-to-number (index &optional (i 0))
  (when (< i (length *number-chinese*))
    (if (containsp (elt *number-chinese* i)
                   index)
        (+ i 1)
        (chinese-index-to-number index
                                 (+ i 1)))))

(defun predict-intent (text)
  (let ((res (handle-data
              (intent-post text))))
    (log:info "predict: ~A~%" res)
    res))

(defun test ()
  (handle-data (intent-post "第十个mc服务器, 启动")))

(in-package :cl-user)
