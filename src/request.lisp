(in-package #:iomux-acceptor)

(defclass iomux-request (hunchentoot:request)
  ())

(defmethod hunchentoot:process-request ((request iomux-request))
  (hunchentoot:handle-request hunchentoot:*acceptor* request))
