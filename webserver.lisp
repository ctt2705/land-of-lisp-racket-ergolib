(load "ergolib/init")
(require :ergolib)
(define-synonym equal? equal)
(define-synonym add1 1+)
(define-synonym display princ)
(def-bb-clause :with-stream (var init &body body)
  `(with-open-stream (,var ,init) (%bb ,@body)))

(defmethod ->list ((s string)) (coerce s 'list))

(defun http-char (c1 c2 &optional (default #\space))
  (bb code (parse-integer (strcat c1 c2)
                          :radix 16
                          :junk-allowed t)
    (if code
      (code-char code)
      default
    )))

(defun decode-param (s)
  (join (iterate f ( (lst (->list s)) )
    (when lst
      (case (car lst)
        (#\% (cons (http-char (cadr lst) (caddr lst))
                   (f (cdddr lst))))
        (#\+ (cons #\space
                   (f (cdr lst))))
        (otherwise (cons (car lst)
                         (f (cdr lst))))
      )))))

(defun parse-params (s &optional (dct (dict)))
  (bb i1 (position #\= s)
      i2 (position #\& s)
    (if (not i1)
      dct
      (bb k (intern (string-upcase (subseq s 0 i1)))
          v (decode-param (subseq s (add1 i1) i2))
        (setref dct k v)
        (if (not i2)
          dct
          (parse-params (subseq s (add1 i2)) dct)
        )))))

(defun parse-url (s)
  (bb url (subseq s 
                  (+ 2 (position #\space s))
                  (position #\space s :from-end t))
      x (position #\? url)
    (if x
      (cons (subseq url 0 x)
            (parse-params (subseq url (add1 x))))
      (cons url (dict))
    )))

(defun get-header (stream &optional (dct (dict)))
  (bb s (read-line stream)
      i (position #\: s)
    (if (not i)
      dct
      (bb k (intern (string-upcase (subseq s 0 i)))
          v (subseq s (+ i 2))
        (setref dct k v)
        (get-header stream dct)
      ))))

(defun get-content-params (stream header)
  (bb length (ref header 'content-length)
    (if (not length)
      (dict)
      (bb content (make-string (parse-integer length))
        (read-sequence content stream)
        (parse-params content)
      ))))

(defun serve (request-handler)
  (bb socket (socket-server 8080)
    (unwind-protect
      (loop (bb :with-stream stream (socket-accept socket)
                url    (parse-url (read-line stream))
                path   (car url)
                header (get-header stream)
                params (copy-into (cdr url)
                                  (get-content-params stream header))
                *standard-output* stream
              (funcall request-handler path header params)
            ))
      (socket-server-close socket)
    )))

(defun hello-request-handler (path header params)
  ;; The following 3 lines needed to be added for my
  ;;  firefox 83.0 to display properly
  ;; References:
  ;;  https://groups.google.com/g/land-of-lisp/c/afwjs0eic94/m/YjbEI2S48iEJ
  ;;  https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Server_response
  (display "HTTP/1.1 200 OK")
  (terpri)
  (terpri)

  (if (equal? path "greeting")
    (bb name (ref params 'name)
      (if (not name)
        (display "<form>What is your name?<input name='name' /></form>")
        (format t "Nice to meet you, ~a!" name)
      ))
    (display "Sorry... I don't know that page.")
  ))

