(defpackage :tel-bot.head
  (:import-from :random-state :make-generator)
  (:import-from :random-state :random-int)
  (:import-from :lzputils.json :load-json-file)
  (:use :common-lisp :babel :str :local-time :lzputils.used)
  (:export
   :start-patron
   :stop-patron
   :create-job

   :random-int-r
   :random-select-list

   :split-s
   :string-merge
   :string-merges

   :generate-path

   :now-today
   :today-format

   :get-source-dir
   :set-source-dir
   :get-data-dir
   :get-config-dir

   :get-configs

   :make-file
   :download-url))
(in-package :tel-bot.head)

(defvar *patron* (make-instance 'patron:patron
                                :worker-capacity 3
                                :job-capacity 32
                                :worker-timeout-duration 600))

(defun start-patron ()
  (patron:start-patron *patron*))

(defun stop-patron ()
  (patron:stop-patron *patron* :wait t))

(defun create-job (fun)
  (patron:submit-job *patron*
                     (make-instance 'patron:job
                                    :function fun)))

(defparameter *source-dir* #P"~/tel_bot/")

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
