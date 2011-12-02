(in-package #:iomux-acceptor)

(defun recv-request-data (socket recv-buf cont)
  (recv-line
   socket recv-buf
   (lambda (stream)
     (let ((first-line (chunga:read-line* stream)))
       (when first-line
         (unless (every #'hunchentoot::printable-ascii-char-p first-line)
           (hunchentoot::send-bad-request-response stream "Non-ASCII character in request line")
           (return-from recv-request-data nil))
         (destructuring-bind (&optional method url-string protocol)
             (ppcre:split "\\s+" first-line :limit 3)
           (unless url-string
             (hunchentoot::send-bad-request-response stream)
             (return-from recv-request-data nil))
           (flet ((handle-headers (headers)
                    (unless protocol (setq protocol "HTTP/0.9"))
                    (funcall cont headers (chunga:as-keyword method) url-string (chunga:as-keyword (chunga:trim-whitespace protocol)))))
             (if protocol
                 (recv-headers
                  socket recv-buf
                  (lambda (stream)
                    (handle-headers (chunga:read-http-headers stream))))
                 (handle-headers nil)))))))))
