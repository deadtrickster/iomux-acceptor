(in-package #:iomux-acceptor)

(defvar *event-base* (make-instance 'iomux:event-base))

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets ()
  '(vector octet))

;; arbitrary, really, but this feels traditional
(defconstant +page-size+ 4096)

(define-constant +crlf/crlf+
    (make-array 4
                :element-type 'octet
                :initial-contents (mapcar 'char-code '(#\Return #\Linefeed #\Return #\Linefeed)))
  :test #'equalp)

(defmacro with-restored-specials (&body body)
  (declare (ignore body))
  (error "not implemented"))

(defmacro with-saved-specials ((&rest specials) &body body)
  (let ((specials (mapcar #'ensure-list specials))
        (inner (gensym "INNER")))
    (dolist (var '(hunchentoot:*acceptor*
                   hunchentoot:*reply*
                   hunchentoot:*request*
                   hunchentoot:*session*))
      (unless (find var specials :key #'first)
        (push (list var) specials)))
    (let ((symbols (mapcar (lambda (special)
                             (declare (ignore special))
                             (gensym "SAVE"))
                           specials)))
      `(let ,(mapcar (lambda (symbol special)
                       (list symbol (or (second special) (first special))))
                     symbols specials)
         (macrolet ((with-restored-specials (&body ,inner)
                       `(let ,',(mapcar (lambda (symbol special)
                                          (list (first special) symbol))
                                        symbols specials)
                          ,@,inner)))
           ,@body)))))

(defmacro safe-lambda ((&rest vars) &body body)
  `(with-saved-specials ()
     (lambda (,@vars)
       (with-restored-specials
         ,@body))))

(defun send-bytes (socket bytes cont)
  (let ((sent 0))
    (labels ((self (fd event exception)
               (declare (ignore event exception))
               (handler-case
                   (incf sent (sockets:send-to socket bytes :start sent :dont-wait t))
                 ;; will generally be a hangup, but whatever...
                 (condition (c)
                   (done c)))
               (when (>= sent (length bytes))
                 (done)))
             (done (&optional condition)
               (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd socket) :write t)
               (if condition
                   (funcall cont condition)
                   (funcall cont))))
      (iomux:set-io-handler *event-base* (sockets:socket-os-fd socket) :write #'self))))

(defclass recv-buf ()
  ((buffer
    :initform (make-array (* 2 +page-size+) :element-type 'octet :adjustable t))
   (read
    :initform 0)
   (write
    :initform 0)))

(defun recv-available (recv-buf)
  (with-slots (read write)
      recv-buf
    (- write read)))

(defun recv-stream (recv-buf &optional length)
  (with-slots (buffer read write)
      recv-buf
    (let ((length (or length (- write read))))
      (let ((start read)
            (end (+ read length)))
        (setf read end)
        (flex:make-in-memory-input-stream buffer :start start :end end)))))

(defun ensure-space (recv-buf &optional (num-bytes +page-size+))
  (with-slots (buffer read write)
      recv-buf
    ;; shift down
    (when (and (plusp read)
               (< (- (length buffer) write) num-bytes))
      (loop
         for from from read below write
         for to from 0
         do (setf (aref buffer to) (aref buffer from)))
      (psetf read 0
             write (- write read)))
    ;; grow
    (when (< (- (length buffer) write) num-bytes)
      (adjust-array buffer (+ (length buffer) num-bytes)))
    recv-buf))

;; receive-from can throw end-of-file
(defun recv-some (socket recv-buf &optional (num-bytes +page-size+))
  (with-slots (buffer read write)
      (ensure-space recv-buf num-bytes)
    (multiple-value-bind (_ nbytes)
        (sockets:receive-from socket :buffer buffer :start write :dont-wait t)
      (declare (ignore _))
      (incf write nbytes))
    recv-buf))

(defun position-of-delimiter (recv-buf delimiter)
  (declare (octets delimiter))
  (with-slots (buffer read write)
      recv-buf
    (when (>= (- write read) (length delimiter))
      (loop
         with end = (1+ (- write (length delimiter)))
         for start = read then (1+ pos)
         for pos = (position (aref delimiter 0) buffer :start start :end end)
         while pos
         thereis (and (loop
                         for d across delimiter
                         for p from pos
                         always (= d (aref buffer p)))
                      (- pos read))))))

(defun stream-to-delimiter (recv-buf delimiter)
  (declare (octets delimiter))
  (when-let ((pos (position-of-delimiter recv-buf delimiter)))
    (recv-stream recv-buf (+ pos (length delimiter)))))

(defun recv-delimited (socket recv-buf delimiter cont)
  (declare (octets delimiter))
  (labels ((self (fd event exception)
             (declare (ignore event exception))
             (handler-case
                 (recv-some socket recv-buf)
               (end-of-file ()
                 nil))
             (when-let ((stream (or (stream-to-delimiter recv-buf delimiter)
                                    (recv-stream recv-buf))))
               (iomux:remove-fd-handlers *event-base* fd :read t)
               (funcall cont stream))))
    (if-let ((stream (stream-to-delimiter recv-buf delimiter)))
      (funcall cont stream)
      (iomux:set-io-handler *event-base* (sockets:socket-os-fd socket) :read #'self))))

(defun recv-line (socket recv-buf cont)
  (recv-delimited socket recv-buf hunchentoot::+crlf+ cont))

;; TODO: end-of-file
(defun recv-headers (socket recv-buf cont)
  (let ((pos (position-of-delimiter recv-buf hunchentoot::+crlf+)))
    (if (zerop pos)
        (funcall cont (recv-stream recv-buf (length hunchentoot::+crlf+)))
        (recv-delimited socket recv-buf +crlf/crlf+ cont))))

(defun recv-content (socket recv-buf length cont)
  (unless length
    (setf length 0))
  (labels ((self (fd event exception)
             (declare (ignore event exception))
             (recv-some socket recv-buf (- length (recv-available recv-buf)))
             (when (>= (recv-available recv-buf) length)
               (iomux:remove-fd-handlers *event-base* fd :read t)
               (funcall cont (recv-stream recv-buf length)))))
    (if (>= (recv-available recv-buf) length)
        (funcall cont (recv-stream recv-buf length))
        (iomux:set-io-handler *event-base* (sockets:socket-os-fd socket) :read #'self))))
