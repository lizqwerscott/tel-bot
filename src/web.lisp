(defpackage :tel-bot.web
  (:import-from :quri :make-uri)
  (:use :cl :tel-bot.head :babel :yason)
  (:export
   :generate-url
   :web-get
   :web-get-url
   :web-post))
(in-package :tel-bot.web)

(defun generate-url (host command &key (args nil) (ssl nil))
  (let ((url (format nil "~A/~A" host command))
        (str-args ""))
    (if (> (length args) 0)
        (setf url
              (format nil "~A?" url)))
    (if ssl
        (setf url
              (format nil "https://~A" url))
        (setf url
              (format nil "http://~A" url)))
    (dolist (i args)
      (setf str-args (string-merge str-args (string-merge (car i) (cdr i) "=") "&")))
    (format nil "~A~A" url str-args)))

(defun make-url (host command args)
  (make-uri :defaults (generate-url host command)
            :query args))

(defun web-get (host command &key args (jsonp nil))
  (let ((text (dex:get (make-url host command args))))
    (if jsonp
        (parse text)
        text)))

(defun web-get-url (url &key (jsonp nil))
  (let ((text (dex:get url)))
    (if jsonp
        (parse text)
        text)))

(defun web-post (url &key args (jsonp t))
  (multiple-value-bind (body status respone-headers uri stream)
      (dex:post url
                :content args)
    (declare (ignorable status uri stream))
    (if (and jsonp
             (str:starts-with-p "application/json"
                                (gethash "content-type"
                                         respone-headers)))
        (parse body)
        body)))

(in-package :cl-user)
