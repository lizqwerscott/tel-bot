(defsystem "tel-bot"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("dexador"
               "alexandria"
               "quri"
               "str"
               "local-time"
               "bordeaux-threads"
               "babel"
               "patron"
               "cl-telegram-bot"
               "random-state"
               "lzputils"
               "easy-config")
  :components ((:module "src"
                :components
                ((:file "head")
                 (:file "web")
                 (:file "chatgpt")
                 (:file "intent")
                 (:file "bot")
                 (:file "task")
                 (:file "text")
                 (:file "music")
                 (:file "picture")
                 (:file "minecraft")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "tel-bot/tests"))))

(defsystem "tel-bot/tests"
  :author ""
  :license ""
  :depends-on ("tel-bot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for tel-bot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
