(defpackage :tel-bot.text
  (:import-from :lzputils.json :assoc-value)
  (:use :common-lisp :tel-bot.bot :tel-bot.web :tel-bot.head :local-time :cl-telegram-bot)
  (:export
   :get-random-text
   :zaoan
   :wanan
   :holidays
   :english-line))
(in-package :tel-bot.text)

(defvar *tian-address* "api.tianapi.com")
(defvar *tianx-key* "f07a432f84956febe20375736114244e")

(defun handle-tianx (data)
  (if (= 200 (assoc-value data "code"))
      (assoc-value data "newslist")
      (let ((errormsg (format nil
                              "[error][tianx]:~A"
                              (assoc-value data "msg"))))
        (log:error "~A~%" errormsg)
        (error errormsg))))

(defun get-zw (path)
  (assoc-value (car
                (handle-tianx
                 (web-get *tian-address*
                          (format nil "~A/index" path)
                          :args `(("key" . ,*tianx-key*))
                          :jsonp t)))
               "content"))

(defun zaoan ()
  (get-zw "zaoan"))

(defun wanan ()
  (get-zw "wanan"))

;; joke sao love
(defun get-random-text (command)
  (web-get "api.vvhan.com" (format nil "api/~A" command)))

(defun holidays ()
  (car
   (handle-tianx
    (web-get *tian-address*
             "jiejiari/index"
             :args `(("key" . ,*tianx-key*)
                     ("date" . ,(today-format))
                     ("mode" . 1))
             :jsonp t))))

(defun english-line ()
  (car
   (handle-tianx
    (web-get *tian-address*
             "everyday/index"
             :args `(("key" . ,*tianx-key*))
             :jsonp t))))

(defun get-story (&key (story-type 2))
  "type 故事类型，成语1、睡前2、童话3、寓言4, word 故事标题"
  (car
   (handle-tianx
    (web-get *tian-address*
             "story/index"
             :args `(("key" . ,*tianx-key*)
                     ("storytype" . ,story-type))
             :jsonp t))))

(defvar *ybanapi* "ybapi.cn")

(defun parse-text-show (text)
  "\\n \r 正确解析"
  (format nil (replace-all-l "\\r" "~%~%%" (replace-all-l "\\\n" "~%" text))))

(defun get-fd-text (name)
  "name: 发颠的对象"
  (parse-text-show
   (web-get *ybanapi*
            "API/fd.php"
            :args `(("name" . ,name)))))

(defun get-cp-text (cp1 cp0)
  "cp1: 攻, cp0: 受"
  (parse-text-show
   (web-get *ybanapi*
            "API/cp.php"
            :args `(("cp1" . ,cp1)
                    ("cp0" . ,cp0)))))

(defun get-acgyiyan ()
  "输出 acg 名言"
  (parse-text-show
   (web-get *ybanapi*
            "API/acgyiyan.php")))

(defun get-kfc ()
  "输出 kfc 文案"
  (parse-text-show
   (web-get *ybanapi*
            "API/kfc.php")))

(defcommand
    (:love "发送一段情话" chat text)
    (declare (ignorable text))
  (handler-case
      (let ((text (get-random-text "love")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:sao "发送一段骚话" chat text)
    (declare (ignorable text))
    (handler-case
      (let ((text (get-random-text "sao")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:joke "发送一段笑话" chat text)
    (declare (ignorable text))
  (handler-case
      (let ((text (get-random-text "joke")))
        (reply text))
    (error (c)
      (reply (format nil "[Error]: ~A" c)))))

(defun handle-story (story)
  (format nil
          "~A~%~A"
          (assoc-value story "title")
          (assoc-value story "content")))

(defcommand
    (:story "发送一段故事(参数: 成语, 睡前, 童话, 寓言)" chat text)
    (handler-case
        (reply
         (let ((match-res (cond
                            ((string= text "成语")
                             1)
                            ((string= text "睡前")
                             2)
                            ((string= text "童话")
                             3)
                            ((string= text "寓言")
                             4)
                            (t
                             5))))
           (if (string= text "")
               (handle-story
                (get-story))
               (if (= match-res 5)
                   "只支持成语、睡前、童话、寓言"
                   (handle-story
                    (get-story :story-type match-res))))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:fd "发颠(参数: 指定发颠名字)" chat text)
    (handler-case
        (reply
         (if (string= text "")
             "请输入发颠的对象"
             (get-fd-text text)))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:acgyiyan "返回 ACG 名言" chat text)
    (declare (ignorable text))
    (handler-case
        (reply
         (get-acgyiyan))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:kfc "返回 Kfc 疯狂星期四文案" chat text)
    (declare (ignorable text))
    (handler-case
        (reply
         (get-kfc))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(defcommand
    (:cp "返回 Cp 文案: 参数(第一个参数: 攻, 第二个参数: 受)" chat text)
    (handler-case
        (reply
         (let ((temp (split-s text)))
           (let ((cp1 (first temp))
                 (cp0 (second temp)))
             (if (and cp1 cp0)
                 (get-cp-text cp1 cp0)
                 "参数不正确: 第一个参数: 攻, 第二个参数: 受"))))
      (error (c)
        (reply (format nil "[Error]: ~A" c)))))

(in-package :cl-user)
