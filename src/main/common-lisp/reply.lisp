(in-package #:iomux-acceptor)

(defclass iomux-reply (hunchentoot:reply)
  ((connection
    :reader reply-connection
    :initarg :connection)))

(defun reply-connection* (&optional (reply hunchentoot:*reply*))
  (reply-connection reply))

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
    bytes))

(defun/cc %iomux-start-reply (&optional (content nil content-provided-p))
  (let/cc cont
    (let ((stream (flex:make-in-memory-output-stream)))
      (let ((hunchentoot::*hunchentoot-stream* stream)
            (hunchentoot::*headers-sent* nil)
            (return-code (hunchentoot:return-code hunchentoot:*reply*)))
        (if content-provided-p
            (hunchentoot:start-output return-code content)
            (hunchentoot:start-output return-code))
        (let ((bytes (flex:get-output-stream-sequence stream)))
          (cl-cont::funcall/cc 'send-bytes cont (reply-connection*) bytes))))))

(defun/cc iomux-start-reply ()
  (%iomux-start-reply))

(defun %iomux-finish-reply (cnn)
  (let ((socket (connection-socket cnn)))
    (ignore-errors
      (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd socket)))
    (close socket)))

(defun/cc iomux-send-chunk (cnn &optional bytes)
  (send-bytes cnn
   (if (null bytes)
       (trivial-utf-8 '(#\0 #\Return #\Linefeed #\Return #\Linefeed))
       (let ((length (utf-8 (format nil "~X~C~C" (length bytes) #\Return #\Linefeed))))
         (concat-bytes length bytes +crlf+)))))

(defun/cc iomux-finish-reply (cnn)
  (iomux-send-chunk cnn)
  (%iomux-finish-reply cnn))

(defun/cc iomux-send-reply (&optional (content nil content-provided-p))
  (let ((cnn (reply-connection*)))
    (if content-provided-p
        (%iomux-start-reply content)
        (%iomux-start-reply))
    (%iomux-finish-reply cnn)))

(defun/cc iomux-send-concat (cnn &rest args)
  (iomux-send-chunk cnn (apply #'concat-bytes args)))

(defun/cc iomux-send-utf-8 (cnn string)
  (iomux-send-chunk cnn (utf-8 string)))

(defmacro iomux-send-stream ((stream) cnn &body body)
  `(iomux-send-utf-8 ,cnn
    (with-output-to-string (,stream)
      ,@body)))

(defun/cc iomux-send-fmt (cnn fmt &rest args)
  (iomux-send-stream (s)
      cnn
    (apply #'format s fmt args)))
