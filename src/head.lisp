(defpackage :tel-bot.head
  (:import-from :random-state :make-generator)
  (:import-from :random-state :random-int)
  (:import-from :lzputils.json :load-json-file)
  (:use :common-lisp :babel :str :local-time :lzputils.used :easy-config)
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

   :make-file
   :file-url-filename
   :download-url

   :replace-all-l))
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

(defun make-file (name extension path)
  (merge-pathnames (format nil "~A.~A" name extension)
                   path))

(defun file-url-filename (url)
  (last1 (split-s url "/")))

(defun download-url (url save-path &optional (proxy nil))
  (handler-case
      (progn
        (run-shell
         (format nil
                 "~Acurl -L ~A -o ~A"
                 (if proxy
                     "proxychains "
                     "")
                 url
                 save-path))
        save-path)
    (error (c)
      (log:error "[download-url Error]: ~A" c))))

(defconfig (:tel-bot t)
    ((bot-token :type :str)
     (proxy :type :str)
     (mc-address :type :str)
     (mc-token :type :str)
     (chatgpt-base-url :default "https://api/openai.com/v1")
     (chatgpt-model :default "gpt-3.5-turbo")
     (chatgpt :type :str)
     (chatgpt-proxy :default nil)
     (intent-address :default "http://127.0.0.1:9797")
     (wxp :default nil)
     (audiop :default nil)
     (wx-host :type :str :default "127.0.0.1")
     (wx-port :type :number :default 5757)
     (picture-port :type :number :default 5556)
     (save-path :type :str)
     (birthday-address :default "127.0.0.1:22400")))

(defun replace-all-l (olds new s)
  (if olds
      (if (listp olds)
          (replace-all-l (cdr olds)
                         new
                         (replace-all (car olds)
                                      new
                                      s))
          (replace-all olds
                       new
                       s))
      s))

(in-package :cl-user)
