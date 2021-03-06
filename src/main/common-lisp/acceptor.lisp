(in-package #:iomux-acceptor)

;; Anything that wants to stay alive needs to heartbeat at least every
;; 30 seconds or risk getting cut-off by an intermediary anyway.
(setf hunchentoot:*default-connection-timeout* 30)

(defclass iomux-acceptor-mixin (iolib-acceptor-mixin) ()
  (:default-initargs
   :request-class 'iomux-request
   :reply-class 'iomux-reply
   :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor iomux-acceptor-mixin) request)
  (call-next-method))

(defun make-listen-handler (acceptor socket)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (when-let ((client-connection (handler-case
                                      (sockets:accept-connection socket :wait t)
                                    (sockets:socket-connection-aborted-error ()))))
      (hunchentoot::set-timeouts client-connection
                                 (hunchentoot:acceptor-read-timeout acceptor)
                                 (hunchentoot:acceptor-write-timeout acceptor))
      (hunchentoot:handle-incoming-connection
       (hunchentoot::acceptor-taskmaster acceptor) client-connection))))

(defmethod hunchentoot:start-listening :around ((acceptor iomux-acceptor-mixin))
  (call-next-method)
  (let ((listener (hunchentoot::acceptor-listen-socket acceptor)))
    (iomux:set-io-handler *event-base*
                          (sockets:socket-os-fd listener)
                          :read
                          (make-listen-handler acceptor listener)))
  (values))

(defmethod hunchentoot:accept-connections :around ((acceptor iomux-acceptor-mixin))
  (let ((listener (hunchentoot::acceptor-listen-socket acceptor)))
    (unwind-protect
         (loop
            (when (hunchentoot::acceptor-shutdown-p acceptor)
              (return))
            (handler-case
                (let ((timeout (iomux:exit-event-loop *event-base* :delay hunchentoot:+new-connection-wait-time+)))
                  (unwind-protect
                       (iomux:event-dispatch *event-base* :one-shot t)
                    (iomux:remove-timer *event-base* timeout)))
              (sockets:socket-connection-reset-error ())))
      (iomux:remove-fd-handlers *event-base* (sockets:socket-os-fd listener))
      (close listener))))

(defun make-connection-handler (acceptor socket)
  (let ((cnn (make-instance 'connection :socket socket)))
    (lambda (fd event exception)
      (declare (ignore event exception))
      (iomux:remove-fd-handlers *event-base* fd :read t)
      (with-call/cc
        (multiple-value-bind (headers-in method url-string protocol)
            (recv-request-data cnn)
          (let* ((content-length (cdr (assoc :content-length headers-in)))
                 (content-stream (recv-content cnn (and content-length (parse-integer content-length)))))
            (without-call/cc
              (let ((hunchentoot:*acceptor* acceptor)
                    (hunchentoot:*reply*    (make-instance (hunchentoot:acceptor-reply-class acceptor) :connection cnn))
                    (hunchentoot:*session*  nil))
                (hunchentoot:process-request
                 (make-instance (hunchentoot:acceptor-request-class acceptor)
                                :acceptor acceptor
                                :remote-addr (sockets:remote-host socket)
                                :remote-port (sockets:remote-port socket)
                                :headers-in headers-in
                                :content-stream content-stream
                                :method method
                                :uri url-string
                                :server-protocol protocol))))))))))

(defmethod hunchentoot:process-connection ((acceptor iomux-acceptor-mixin) (socket t))
  (iomux:set-io-handler *event-base*
                        (sockets:socket-os-fd socket)
                        :read
                        (make-connection-handler acceptor socket))
  (iomux:set-error-handler *event-base*
                           (sockets:socket-os-fd socket)
                           (lambda (fd event)
                             (declare (ignore event))
                             (iomux:remove-fd-handlers *event-base* fd)
                             (close socket :abort t))))

(defmethod hunchentoot:handle-request ((acceptor iomux-acceptor-mixin) (request iomux-request))
  (hunchentoot:acceptor-dispatch-request acceptor request))

(defun hunchentoot::input-chunking-p ()
  nil)
