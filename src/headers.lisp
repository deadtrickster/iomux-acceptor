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

(defun hunchentoot:start-output (return-code &optional (content nil content-provided-p))
  (let* ((chunkedp (and (hunchentoot:acceptor-output-chunking-p hunchentoot:*acceptor*)
                        (eq (hunchentoot:server-protocol hunchentoot:*request*) :http/1.1)
                        ;; only turn chunking on if the content
                        ;; length is unknown at this point...
                        (null (or (hunchentoot:content-length*) content-provided-p))))
         (request-method (hunchentoot:request-method hunchentoot:*request*))
         (head-request-p (eq request-method :head))
         content-modified-p)
    (multiple-value-bind (keep-alive-p keep-alive-requested-p)
        (hunchentoot::keep-alive-p hunchentoot:*request*)
      (when keep-alive-p
        (setq keep-alive-p
              ;; use keep-alive if there's a way for the client to
              ;; determine when all content is sent (or if there
              ;; is no content)
              (or chunkedp
                  head-request-p
                  (eql (hunchentoot:return-code*) hunchentoot:+http-not-modified+)
                  (hunchentoot:content-length*)
                  content)))
      ;; now set headers for keep-alive and chunking
      (when chunkedp
        (setf (hunchentoot:header-out :transfer-encoding) "chunked"))
      (cond ((hunchentoot::header-out-set-p :connection)
             (setf (hunchentoot::*close-hunchentoot-stream*) nil))
            (keep-alive-p
             (setf hunchentoot::*close-hunchentoot-stream* nil)
             (when (and (hunchentoot:acceptor-read-timeout hunchentoot:*acceptor*)
                        (or (not (eq (hunchentoot:server-protocol hunchentoot:*request*) :http/1.1))
                            keep-alive-requested-p))
               ;; persistent connections are implicitly assumed for
               ;; HTTP/1.1, but we return a 'Keep-Alive' header if the
               ;; client has explicitly asked for one
               (setf (hunchentoot:header-out :connection) "Keep-Alive"
                     (hunchentoot:header-out :keep-alive)
                     (format nil "timeout=~D" (hunchentoot:acceptor-read-timeout hunchentoot:*acceptor*)))))
            (t (setf (hunchentoot:header-out :connection) "Close"))))
    (unless (and (hunchentoot::header-out-set-p :server)
                 (null (hunchentoot:header-out :server)))
      (setf (hunchentoot:header-out :server)
            (or (hunchentoot:header-out :server)
                (hunchentoot::acceptor-server-name hunchentoot:*acceptor*))))
    (setf (hunchentoot:header-out :date) (hunchentoot:rfc-1123-date))
    (when (and (stringp content)
               (not content-modified-p)
               (hunchentoot::starts-with-one-of-p (or (hunchentoot:content-type*) "")
                                                  hunchentoot:*content-types-for-url-rewrite*))
      ;; if the Content-Type header starts with one of the strings
      ;; in *CONTENT-TYPES-FOR-URL-REWRITE* then maybe rewrite the
      ;; content
      (setq content (hunchentoot::maybe-rewrite-urls-for-session content)))
    (when (stringp content)
      ;; if the content is a string, convert it to the proper external format
      (setf content (hunchentoot::string-to-octets content :external-format (hunchentoot:reply-external-format*))
            (hunchentoot:content-type*)
            (hunchentoot::maybe-add-charset-to-content-type-header (hunchentoot:content-type*)
                                                                   (hunchentoot:reply-external-format*))))
    (when content
      ;; whenever we know what we're going to send out as content, set
      ;; the Content-Length header properly; maybe the user specified
      ;; a different content length, but that will wrong anyway
      (setf (hunchentoot:header-out :content-length) (length content)))
    ;; send headers only once
    (when hunchentoot::*headers-sent*
      (return-from hunchentoot:start-output))
    (setq hunchentoot::*headers-sent* t)
    (hunchentoot::send-response hunchentoot:*acceptor*
                                hunchentoot::*hunchentoot-stream*
                                return-code
                                :headers (hunchentoot:headers-out*)
                                :cookies (hunchentoot:cookies-out*)
                                :content (unless head-request-p
                                           content))
    ;; when processing a HEAD request, exit to return from PROCESS-REQUEST
    (when head-request-p
      (throw 'hunchentoot::request-processed nil))
    (when chunkedp
      ;; turn chunking on after the headers have been sent
      (unless (typep hunchentoot::*hunchentoot-stream* 'chunga:chunked-stream)
        (setq hunchentoot::*hunchentoot-stream* (chunga:make-chunked-stream hunchentoot::*hunchentoot-stream*)))
      (setf (chunga:chunked-stream-output-chunking-p hunchentoot::*hunchentoot-stream*) t))
    hunchentoot::*hunchentoot-stream*))
