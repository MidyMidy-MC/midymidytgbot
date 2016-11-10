(in-package :midymidybot)

(defun string-begin-with (match string)
  (let ((len (length match)))
    (and (>= (length string) len)
         (string= match
                  (subseq string 0 len)))))

(defun time-str ()
  (multiple-value-bind
        (ss mm hh d m)
      (get-decoded-time)
    (format nil "[~A-~A ~2,'0D:~2,'0D:~2,'0D]"
            m d hh mm ss)))

(defun logging (msg &rest args)
  (apply #'format
         (append
          (list *log-out*
                (concatenate 'string
                             (time-str) " " msg "~%"))
          args)))

(defun exit ()
  (sb-ext:exit))

(defun make-str ()
  (make-array '(0) :element-type 'character
              :fill-pointer 0 :adjustable t))

(defmacro tryto (&rest body)
  `(handler-case (progn ,@body)
     (condition () :err)))

(defun correct-cljson-surrogate-pairs (wrong-string)
  (with-output-to-string (out)
    (let ((len (length wrong-string)))
      (dotimes (i len)
        (let* ((char1 (aref wrong-string i))
               (c1 (char-code char1)))
          (if (not (and (>= c1 #xD800)
                        (<= c1 #xDBFF)))
              (write-char char1 out)
              (block nil
                (if (>= (1+ i) len)
                    (progn
                      (logging "Corrector: invalid input, A8BSP")
                      (return "[Invalid JSON, code: LXEA6FD]"))
                    (incf i))
                (let ((c2 (char-code (aref wrong-string i))))
                  (write-char
                   (code-char
                    (+ #x10000
                       (ash (logand #x03FF c1) 10)
                       (logand #x03FF c2)))
                   out)))))))))
;; (correct-cljson-surrogate-pairs
;;  (with-input-from-string
;;      (stream "\"你好\\uD83D\\uDE03吼啊\"")
;;    (cl-json:decode-json stream)))

(defun upload-binary-file (byte-vector)
  (let ((boundary (format nil "~A"
                          (random 10000000000000)))
        (str (make-str)))
    (with-output-to-string (out str)
      (write-line (format nil "--~A" boundary) out)
      (write-line "Content-Disposition: form-data; name=\"image\"; filename=\"image\"" out)
      (write-line "Content-Type: application/octet-stream" out)
      (write-line "" out))
    (let* ((sa (flexi-streams:string-to-octets
                str :external-format '(:utf-8 :eol-style :crlf)))
           (byte-stream (flexi-streams:make-in-memory-output-stream
                         :element-type '(unsigned-byte 8))))
      (write-sequence sa byte-stream)
      (write-sequence byte-vector byte-stream)
      (write-sequence (flexi-streams:string-to-octets
                       (format nil "~%--~A--~%" boundary)
                       :external-format '(:utf-8 :eol-style :crlf))
                      byte-stream)
      (let ((out (flexi-streams:get-output-stream-sequence
                  byte-stream)))
        (http-request
         "https://img.vim-cn.com/"
         :method :post
         :content-type
         (format nil
                 "multipart/form-data; boundary=~A" boundary)
         :content-length (length out)
         :content out)))))
