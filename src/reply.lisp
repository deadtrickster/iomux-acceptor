(in-package #:iomux-acceptor)

(defclass iomux-reply (hunchentoot:reply)
  ((socket
    :accessor reply-socket
    :initarg :socket)))

;; end of the line
(defun reply-done ()
  (let ((socket (reply-socket hunchentoot:*reply*)))
    (lambda (&rest args)
      (declare (ignore args))
      (ignore-errors
        (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd socket)))
      (ignore-errors
        (close socket)))))

(defun iomux-send-reply (content &optional (cont (reply-done)))
  (let ((hunchentoot::*hunchentoot-stream* (flex:make-in-memory-output-stream))
        (hunchentoot::*headers-sent* nil)
        (return-code (hunchentoot:return-code hunchentoot:*reply*)))
    (hunchentoot::start-output
     return-code
     (or (hunchentoot:acceptor-status-message hunchentoot:*acceptor* return-code)
         content))
    (send-bytes (reply-socket hunchentoot:*reply*)
                (flex:get-output-stream-sequence hunchentoot::*hunchentoot-stream*)
                cont)))
