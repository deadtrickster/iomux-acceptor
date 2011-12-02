(in-package #:iomux-acceptor)

(defclass iomux-reply (hunchentoot:reply)
  ((socket
    :accessor reply-socket
    :initarg :socket)))

(defun iomux-send-reply (content &optional (cont 'done))
  (let ((hunchentoot::*hunchentoot-stream* (flex:make-in-memory-output-stream))
        (return-code (hunchentoot:return-code hunchentoot:*reply*)))
    (hunchentoot::start-output
     return-code
     (or (hunchentoot:acceptor-status-message hunchentoot:*acceptor* return-code)
         content))
    (send-bytes (reply-socket hunchentoot:*reply*)
                (flex:get-output-stream-sequence hunchentoot::*hunchentoot-stream*)
                cont)))
