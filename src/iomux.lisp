(in-package #:iomux-acceptor)

(define-constant +crlf/crlf+
    (make-array 4
                :element-type 'octet
                :initial-contents (mapcar 'char-code '(#\Return #\Linefeed #\Return #\Linefeed)))
  :test #'equalp)

;; for your slime only
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

(defun/cc recv-line (cnn)
  (stream-to-delimited (recv-delimited cnn hunchentoot::+crlf+) hunchentoot::+crlf+))

;; TODO: end-of-file
(defun/cc recv-headers (cnn)
  (stream-to-delimited (recv-delimited cnn +crlf/crlf+) +crlf/crlf+))

(defun/cc recv-content (cnn length)
  (unless length
    (setf length 0))
  (eat-stream (recv-fixed cnn length) length))
