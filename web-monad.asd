
(asdf:defsystem :web-monad
  :description "The Web Monad"
  :author "VIP"
  :serial t
  :depends-on ("vip-utils" "monads" "aserve" "html" "session"
                           "trivial-utf-8"
                           "anaphors" "trivial-backtrace" "parenscript"
                           "cl-ppcre"
                           "web-utils")
  
  :components ((:file "package")
               (:file "web-monad")
               (:file "monadic-web-widgets")
               (:file "multi-pane-form")))
