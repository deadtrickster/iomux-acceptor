(asdf:defsystem #:iomux-acceptor
  :depends-on (#:alexandria
               #:chunga
               #:flexi-streams
               #:hunchentoot
               #:iolib)
  :serial t
  :components
  ((:file "packages")
   (:module :src
            :components
            ((:file "acceptor" :depends-on ("headers" "request"))
             (:file "headers"  :depends-on ("iomux"))
             (:file "iomux")
             (:file "reply"    :depends-on ("iomux"))
             (:file "request"  :depends-on ("reply"))))))
