(asdf:defsystem #:iomux-acceptor
  :description "Evented IO for Hunchentoot."
  :version "0.2"
  :author "Eric Sessoms <eric@nubgames.com>"
  :license "MIT"
  :depends-on (#:cl-base64
               #:iolib-acceptor
               #:ironclad
               #:protocol)
  :components
  ((:module :main
            :pathname "src/main/common-lisp"
            :components
            ((:file "acceptor"  :depends-on ("headers" "request"))
             (:file "headers"   :depends-on ("iomux"))
             (:file "iomux"     :depends-on ("packages"))
             (:file "packages")
             (:file "reply"     :depends-on ("iomux"))
             (:file "request"   :depends-on ("reply"))
             (:file "websocket" :depends-on ("headers" "request"))))))
