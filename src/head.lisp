(defpackage :tel-bot.head
  (:import-from :jonathan :to-json)
  (:import-from :uiop :run-program)
  (:import-from :random-state :make-generator)
  (:import-from :random-state :random-int)
  (:use :common-lisp :yason :babel :str :local-time)
  (:export
   :*patron*
   :run-shell
   :assoc-s
   :assoc-value
   :assoc-value-l
   :assoc-v

   :random-int-r
   :random-select-list

   :split-s
   :string-merge
   :string-merges
   :bits-to-json

   :generate-path
   :when-bind
   :last1
   :append1
   :to-json-a

   :now-today
   :today-format

   :get-source-dir
   :set-source-dir
   :get-data-dir
   :get-config-dir

   :load-json-file
   :save-json-file

   :get-configs
   :include-words

   :make-file
   :download-url))
(in-package :tel-bot.head)

(defvar *patron* (make-instance 'patron:patron
                                :worker-capacity 3
                                :job-capacity 32
                                :worker-timeout-duration 600))

(defparameter *source-dir* #P"~/tel_bot/")

(setf yason:*parse-object-as* :alist)

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defun last1 (lst)
  (car (last lst)))

(defun append1 (lst item)
  (append lst
          (list item)))

(defun assoc-s (plist key)
  (assoc key plist :test #'string=))

(defun assoc-value (plist keys)
  (if (listp keys)
      (if keys
          (assoc-value (cdr
                        (assoc (car keys) plist :test #'string=))
                       (cdr keys))
          plist)
      (cdr (assoc keys plist :test #'string=))))

(defun assoc-value-l (plist keys)
  (when (listp keys)
    (mapcar #'(lambda (key)
                (assoc-value plist key))
            keys)))

(defun random-int-r (max)
  (let ((generator (random-state:make-generator :mersenne-twister-32 (timestamp-to-universal (now)))))
    (random-state:random-int generator 0 max)))

(defun random-select-list (lst)
  (when (listp lst)
    (let ((select (random-int-r (- (length lst) 1))))
      (elt lst select))))

(defun split-s (str &optional (deleimiter " "))
  (split deleimiter str))

(defun string-merge (str1 str2 delimiter)
  (if (or (equal str1 "") (equal str2 ""))
      (format nil "~A~A" str1 str2)
      (format nil "~A~A~A" str1 delimiter str2)))

(defun string-merges (lst &optional (deleimiter ""))
  (if (= (length lst) 1)
      (car lst)
      (join deleimiter lst)))

(defun bits-to-json (bits)
  (parse (babel:octets-to-string bits)))

(defun run-shell (program)
  (run-program program))

(defun to-json-a (alist)
  (to-json alist :from :alist))

(defun now-today ()
  (let ((now-time (now)))
    (encode-timestamp 0 0 0 8
                      (timestamp-day now-time)
                      (timestamp-month now-time)
                      (timestamp-year now-time))))

(defun today-format (&optional (is-chinesep nil))
  (format-timestring nil
                     (today)
                     :format (if is-chinesep
                                 '(:year "年" :month "月" :day "日")
                                 '(:year "-" :month "-" :day))))

(defun get-source-dir ()
  *source-dir*)

(defun set-source-dir (path)
  (setf *source-dir* path))

(defun get-data-dir ()
  (merge-pathnames "datas/"
                   (get-source-dir)))

(defun get-config-dir ()
  (merge-pathnames "configs/"
                   (get-source-dir)))

(defun load-json-file (path)
  (with-open-file (in path :direction :input :if-does-not-exist :error)
    (multiple-value-bind (s) (make-string (file-length in))
      (read-sequence s in)
      (parse s))))

(defun save-json-file (path json)
  (with-open-file (out path
                       :direction :output
                       :if-exists :overwrite
                       :if-does-not-exist :create)
    (write-sequence json out)))

(defvar *configs* nil)

(defun load-config-file ()
  (let ((path (merge-pathnames "config.json"
                               (get-config-dir))))
    (when (probe-file path)
      (setq *configs*
            (load-json-file path)))))

(load-config-file)

(defun get-configs ()
  *configs*)

(defun include-words (text keywords &optional res)
  (if keywords
      (include-words text
                     (cdr keywords)
                     (or res
                         (contains? (car keywords)
                                    text)))
      res))

(defun make-file (name extension path)
  (merge-pathnames (format nil "~A.~A" name extension)
                   path))

(defun download-url (url save-path)
  (handler-case
      (progn
        (run-shell
         (format nil
                 "curl -L ~A -o ~A"
                 url
                 save-path))
        save-path)
    (error (c)
      (format t "[download-url Error]: ~A~%" c)
      nil)))

(in-package :cl-user)
