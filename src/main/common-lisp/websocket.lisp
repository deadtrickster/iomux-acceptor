(in-package #:iomux-acceptor)

;; dGhlIHNhbXBsZSBub25jZQ== -> s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
(let ((guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
  (defun signature (key)
    (base64:usb8-array-to-base64-string
     (ironclad:digest-sequence :sha1 (ironclad:ascii-string-to-byte-array (concatenate 'string key guid))))))

(defun/cc websocket-handshake ()
  (let/cc cont
    (destructuring-bind (connection upgrade key version protocol)
        (mapcar #'hunchentoot:header-in*
                '(:connection :upgrade :sec-websocket-key :sec-websocket-version :sec-websocket-protocol))
      (declare (ignore connection upgrade version))
      (deletef (slot-value hunchentoot:*reply* 'hunchentoot:headers-out) :content-type :key #'car :test #'eq)
      (setf (hunchentoot:return-code*) hunchentoot:+http-switching-protocols+
            (hunchentoot:header-out :connection) "Upgrade"
            (hunchentoot:header-out :upgrade) "websocket"
            (hunchentoot:header-out :sec-websocket-accept) (signature key)
            (hunchentoot:header-out :sec-websocket-protocol) protocol)
      (cl-cont::funcall/cc 'iomux-start-reply cont))))

;;; Base Framing Protocol
;;;
;;;  0                   1                   2                   3
;;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;;; +-+-+-+-+-------+-+-------------+-------------------------------+
;;; |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
;;; |I|S|S|S|  (4)  |A|     (7)     |             (16/63)           |
;;; |N|V|V|V|       |S|             |   (if payload len==126/127)   |
;;; | |1|2|3|       |K|             |                               |
;;; +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
;;; |     Extended payload length continued, if payload len == 127  |
;;; + - - - - - - - - - - - - - - - +-------------------------------+
;;; |                               |Masking-key, if MASK set to 1  |
;;; +-------------------------------+-------------------------------+
;;; | Masking-key (continued)       |          Payload Data         |
;;; +-------------------------------- - - - - - - - - - - - - - - - +
;;; :                     Payload Data continued ...                :
;;; + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
;;; |                     Payload Data continued ...                |
;;; +---------------------------------------------------------------+

;;; Field definitions
(defmacro fin    (word) `(ldb (byte 1 15) ,word))
(defmacro opcode (word) `(ldb (byte 4  8) ,word))
(defmacro mask   (word) `(ldb (byte 1  7) ,word))
(defmacro len    (word) `(ldb (byte 7  0) ,word))

;;; Accessors
(defun finp  (word) (logbitp 15 word))
(defun maskp (word) (logbitp  7 word))

(defsetf finp  (word) (b) `(setf (fin  ,word) (if ,b 1 0)))
(defsetf maskp (word) (b) `(setf (mask ,word) (if ,b 1 0)))

;;; Opcodes
(defconstant +continuation-frame+  0)
(defconstant +text-frame+          1)
(defconstant +binary-frame+        2)
(defconstant +close-frame+         8)
(defconstant +ping-frame+          9)
(defconstant +pong-frame+         10)

(defun/cc websocket-recv-message (cnn)
  (let ((opcode +continuation-frame+)
        (fragments nil))
    (loop
       (multiple-value-bind (finp frame fragment)
           (websocket-recv-frame cnn)
         (unless (= frame +continuation-frame+)
           (setf opcode frame))
         (when fragment
           (push fragment fragments))
         (when finp
           (return-from websocket-recv-message
             (values opcode
                     (and fragments
                          (apply #'concat-bytes (nreverse fragments))))))))))

(defun/cc websocket-recv-frame (cnn)
  (with-recv (cnn)
      ((control :ushort))
    (let ((recipe nil)
          (length 0)
          (mask 0))
      (declare ((unsigned-byte 32) mask))
      (when (= (len control) 126) (push :ushort recipe))
      (when (= (len control) 127) (push :ulong  recipe))
      (when (maskp control) (push :uint recipe))
      (let ((values (ensure-list (apply #'recv-unpack cnn (nreverse recipe)))))
        (setf length (if (> (len control) 125) (pop values) (len control))
              mask (if (maskp control) (pop values) 0)))
      (let ((data (when (plusp length)
                    (eat-to-expected (recv-fixed cnn length)))))
        (without-call/cc
          (when (maskp control)
            (loop
               for i below (length data)
               for j = (- 3 (mod i 4))
               do (setf (aref data i)
                        (logxor (aref data i) (ldb (byte 8 (* j 8)) mask))))))
        (values (finp control) (opcode control) data)))))

(defun/cc websocket-send-frame (cnn finp opcode &optional data)
  (let/cc cont
    (let ((recipe nil)
          (control 0))
      (declare ((unsigned-byte 16) control))
      (when finp
        (setf (ldb (byte 1 15) control) 1))
      (setf (opcode control) opcode)
      (when data
        (push data recipe)
        (let ((length (length data)))
          (cond
            ((> length 65535)
             (push '(:uchar :ulong) recipe)
             (setf (len control) 127))
            ((> length 125)
             (push '(:uchar :ushort) recipe)
             (setf (len control) 126))
            (t
             (push '(:uchar *) recipe)
             (setf (len control) length)))))
      (push control recipe)
      (push :ushort recipe)
      (cl-cont::apply/cc 'send cont cnn recipe))))
