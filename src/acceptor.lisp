(in-package #:iomux-acceptor)

;; mixin hunchentoot:easy-acceptor to use that dispatch framework
(defclass iomux-acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs
   :request-class 'iomux-request
   :reply-class 'iomux-reply
   :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster)))

;; mixin hunchentoot:easy-acceptor to use that dispatch framework
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor iomux-acceptor) request)
  (call-next-method))

(defun make-listen-handler (acceptor socket)
  (lambda (fd event exception)
    (declare (ignore fd event exception))
    (let ((client-connection
           (sockets:accept-connection socket :wait t)))
      (hunchentoot:handle-incoming-connection
       (hunchentoot::acceptor-taskmaster acceptor) client-connection))))

(defmethod hunchentoot:start-listening ((acceptor iomux-acceptor))
  (when (hunchentoot::acceptor-listen-socket acceptor)
    (hunchentoot:hunchentoot-error "acceptor ~A is already listening" acceptor))
  (let ((socket
         (sockets:make-socket :connect :passive
                              :local-port (hunchentoot:acceptor-port acceptor)
                              :reuse-address t
                              :backlog (hunchentoot::acceptor-listen-backlog acceptor))))
    (setf (hunchentoot::acceptor-listen-socket acceptor) socket)
    (iomux:set-io-handler *event-base*
                          (sockets:socket-os-fd socket)
                          :read
                          (make-listen-handler acceptor socket)))
  (values))

(defmethod hunchentoot:accept-connections ((acceptor iomux-acceptor))
  (loop
     (when (hunchentoot::acceptor-shutdown-p acceptor)
       (return))
     (iomux:event-dispatch *event-base* :one-shot t :timeout hunchentoot::+new-connection-wait-time+)))

(defun make-connection-handler (acceptor socket)
  (let ((recv-buf (make-instance 'recv-buf)))
    (lambda (fd event exception)
      (declare (ignore fd event exception))
      (recv-request-data
       socket recv-buf
       (lambda (headers-in method url-string protocol)
         (let ((content-length (cdr (assoc :content-length headers-in))))
           (recv-content
            socket recv-buf content-length
            (lambda (content-stream)
              (let ((hunchentoot:*reply* (make-instance (hunchentoot:acceptor-reply-class acceptor)))
                    (hunchentoot:*session* nil))
                (hunchentoot:process-request
                 (make-instance (hunchentoot:acceptor-request-class acceptor)
                                :acceptor acceptor
                                :remote-addr (sockets:remote-host socket)
                                :remote-port (sockets:remote-port socket)
                                :headers-in headers-in
                                :content-stream content-stream
                                :method method
                                :uri url-string
                                :server-protocol protocol)))))))))))

(defmethod hunchentoot:process-connection ((acceptor iomux-acceptor) (socket t))
  (iomux:set-io-handler *event-base*
                        (sockets:socket-os-fd socket)
                        :read
                        (make-connection-handler acceptor socket)))

(defmethod hunchentoot:handle-request ((acceptor iomux-acceptor) (request iomux-request))
  (hunchentoot:acceptor-dispatch-request acceptor request))
