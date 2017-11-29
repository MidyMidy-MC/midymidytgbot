(in-package :midymidybot)

(defmacro with-timeout-nil (timeout &body body)
  (let ((timer (gensym))
        (value (gensym)))
    `(handler-case
         (let ((,timer (sb-ext:make-timer (lambda ()
                                     (error "Timeout")))))
           (sb-ext:schedule-timer ,timer ,timeout)
           (let ((,value (progn ,@body)))
             (sb-ext:unschedule-timer ,timer)
             ,value))
       (simple-error () nil))))

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

(defun logging (source msg &rest args)
  (let ((str
         (apply #'format
                (append
                 (list nil
                       (concatenate 'string
                                    (time-str)
                                    (format nil "[~A]" source)
                                    msg))
                 args))))
    (write-line str *log-out*)
    (if *log-file*
        (with-open-file (out *log-file* :direction :output
                             :if-exists :append
                             :if-does-not-exist :create
                             :element-type 'character)
          (write-line str out)))))

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
    (with-input-from-string (in wrong-string)
      (do ((c (read-char in nil nil)
              (read-char in nil nil)))
          ((null c))
        (if (<= #xD800 (char-code c) #xDBFF)
            ;; open surrogate pair
            (let ((cc (read-char in nil nil)))
              (if (or (null cc)
                      (not (<= #xDC00 (char-code cc) #xDFFF)))
                  (write-char #\replacement_character out)
                  (let ((c1 (char-code c))
                        (c2 (char-code cc)))
                    (write-char (code-char
                                 (+ #x10000
                                    (ash (logand #x03FF c1) 10)
                                    (logand #x03FF c2)))
                                out))))
            ;; normal characters
            (if (<= #xDC00 (char-code c) #xDFFF)
                (write-char #\replacement_character out)
                (write-char c out)))))))
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

(load "./os-release.lisp")
;; Archlinux: /lib/libcurl.so
;; Debian 8: /usr/lib/x86_64-linux-gnu/libcurl.so
(defun determine-libcurl-path ()
  (let ((os-release (os-release:read-os-release)))
    (cond ((string= "arch" (getf os-release :id))
           #p"/lib/libcurl.so")
          ((string= "debian" (getf os-release :id))
           #p"/usr/lib/x86_64-linux-gnu/libcurl.so")
          ((string= "debian" (getf os-release :id_like))
           #p"/usr/lib/x86_64-linux-gnu/libcurl.so"))))
(cffi:load-foreign-library (determine-libcurl-path))
(cffi:load-foreign-library #p"./c/libuselibcurl.so")

(cffi:defcstruct mem_block
  (mem :pointer)
  (size :unsigned-long))

(cffi:defcfun "global_init_curl" :void)
(global-init-curl)

;; HTTP-GET
(cffi:defcfun "http_get" (:pointer (:struct mem_block))
  (url :pointer))
