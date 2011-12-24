(in-package #:iomux-acceptor)

(defclass iomux-request (hunchentoot:request)
  ())

(defmethod hunchentoot:process-request ((request iomux-request))
  (let ((hunchentoot:*request* request))
    (hunchentoot:handle-request hunchentoot:*acceptor* request)))
