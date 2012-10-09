(in-package #:iomux-acceptor)

(defun/cc recv-request-data (cnn)
  (let* ((stream (recv-line cnn))
         (first-line (chunga:read-line* stream)))
    (when first-line
      (unless (every #'hunchentoot::printable-ascii-char-p first-line)
        (hunchentoot::send-bad-request-response stream "Non-ASCII character in request line"))
      (destructuring-bind (&optional method url-string protocol)
          (ppcre:split "\\s+" first-line :limit 3)
        (unless url-string
          (hunchentoot::send-bad-request-response stream))
        (flet ((handle-headers (headers)
                 (unless protocol (setq protocol "HTTP/0.9"))
                 (values headers (chunga:as-keyword method) url-string (chunga:as-keyword (chunga:trim-whitespace protocol)))))
          (if protocol
              (let ((stream (recv-headers cnn)))
                (handle-headers (chunga:read-http-headers stream)))
              (handle-headers nil)))))))
