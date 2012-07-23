(in-package #:iomux-acceptor)

(defclass iomux-reply (hunchentoot:reply)
  ((connection
    :reader reply-connection
    :initarg :connection)))

(defun utf-8 (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun concat-bytes (&rest args)
  (let* ((length (the fixnum (loop for arg in args sum (length arg))))
         (bytes (make-array length :element-type 'octet))
         (pos 0))
    (declare (fixnum pos))
    (dolist (arg args)
      (declare ((simple-array octet) arg))
      (loop
         for byte across arg
         do (setf (aref bytes pos) byte
                  pos (1+ pos))))
    (setf (aref bytes pos) type)
    bytes))

(defun %iomux-finish-reply ()
  (let ((socket (-> hunchentoot:*reply* reply-connection connection-socket)))
    (ignore-errors
      (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd socket)))
    (close socket)))

(defun/cc %iomux-start-reply (&optional content)
  (let ((hunchentoot::*hunchentoot-stream* (flex:make-in-memory-output-stream))
        (hunchentoot::*headers-sent* nil)
        (return-code (hunchentoot:return-code hunchentoot:*reply*)))
    (hunchentoot::start-output return-code content)
    (send-bytes (reply-connection hunchentoot:*reply*)
                (flex:get-output-stream-sequence hunchentoot::*hunchentoot-stream*))))

(defun/cc iomux-send-chunk (&optional bytes)
  (send-bytes (reply-connection hunchentoot:*reply*)
              (if (null bytes)
                  (trivial-utf-8 '(#\0 #\Return #\Linefeed #\Return #\Linefeed))
                  (let ((length (babel:string-to-octets
                                 (format nil "~X~C~C" (length bytes) #\Return #\Linefeed))))
                    (concat-bytes length bytes +crlf+)))))

(defun/cc iomux-send-concat (&rest args)
  (iomux-send-chunk (apply #'concat-bytes args)))

(defun/cc iomux-send-utf-8 (string)
  (iomux-send-chunk (utf-8 string)))

(defmacro iomux-send-stream ((stream) &body body)
  `(iomux-send-utf-8
    (with-output-to-string (,stream)
      ,@body)))

(defun/cc iomux-send-fmt (fmt &rest args)
  (iomux-send-stream (s)
    (apply #'format s fmt args)))

(defun/cc iomux-finish-reply ()
  (iomux-send-chunk)
  (%iomux-finish-reply))

(defun/cc iomux-start-reply ()
  (%iomux-start-reply))

(defun/cc iomux-send-reply (content)
  (%iomux-start-reply content)
  (%iomux-finish-reply))

(trace %iomux-start-reply %iomux-finish-reply iomux-send-chunk)
