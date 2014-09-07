;;
;; Wizard - Automatic software source package configuration
;; Copyright (C) 2014 Jesse W. Towner
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the authors nor the names of its contributors
;; may be used to endorse or promote products derived from this
;; software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(define path-separator (make-parameter (cond-expand (windows #\\) (else #\/))))

(define (path-separator? character)
  (cond-expand
    (windows
      (or (char=? character #\\)
          (char=? character #\/)))
    (else
      (char=? character #\/))))

(define (path-absolute? path)
  (if (zero? (string-length path))
    #f
    (path-separator? (string-ref path 0))))

(define (path-relative? path)
  (not (path-absolute? path)))

(define (path-trim-leading-separators/position path first last)
  (let loop ((i first))
    (if (= i last)
      last
      (if (path-separator? (string-ref path i))
          (loop (+ i 1))
          i))))

(define (path-trim-trailing-separators/position path first last)
  (let loop ((i last))
    (if (= i first)
      first
      (let ((j (- i 1)))
        (if (path-separator? (string-ref path j))
          (loop j)
          i)))))

(define (path-trim-separators path)
  (let* ((len   (string-length path))
         (first (path-trim-leading-separators/position 0 len))
         (last  (path-trim-trailing-separators/position first len)))
    (if (< first last)
      (string-copy path first last)
      "")))

(define (path-trim-trailing-separators path)
  (let* ((len   (string-length path))
         (last  (path-trim-trailing-separators/position 0 len)))
    (cond ((= last len) path)
          ((> last 0)   (string-copy path 0 last))
          (else         ""))))

(define (path-basename path)
  (let* ((len  (string-length path))
         (last (path-trim-trailing-separators/position 0 len)))
    (if (zero? last)
      ""
      (let loop ((i last))
        (if (zero? i)
          (string-copy path 0 last)
          (let ((j (- i 1)))
            (if (path-separator? (string-ref path j))
              (string-copy path i last)
              (loop j))))))))
        
(define (path-dirname path)
  (let* ((len  (string-length path))
         (last (path-trim-trailing-separators/position 0 len)))
    (if (zero? last)
      (if (< last len)
        (string (path-separator))
        ".")
      (let loop ((i last))
        (if (zero? i)
          "."
          (let ((j (- i 1)))
            (if (path-separator? (string-ref path j))
              (begin
                (set! j (path-trim-trailing-separators/position 0 j))
                (string-copy path 0 (if (zero? j) 1 j)))
              (loop j))))))))

(define (path-extension path)
  (let loop ((i (string-length path)))
    (if (zero? i)
      #f
      (let* ((j (- i 1))
             (c (string-ref path j)))
        (cond
          ((and (char=? c #\.)
                (> j 0)
                (not (path-separator? (string-ref path (- j 1)))))
           (string-copy path i))
          ((path-separator? c)
           #f)
          (else
           (loop j)))))))

(define (path-split path)
  (let* ((len  (string-length path))
         (last (path-trim-trailing-separators/position 0 len)))
    (if (zero? last)
      (if (< last len)
        `(,(string (path-separator)))
        '())
      (let loop ((i last))
        (if (zero? i)
          '(,(string-copy path 0 last))
          (let ((j (- i 1)))
            (if (path-separator? (string-ref path j))
              (append (if (zero? j)
                        `(,(string (path-separator)))
                        (string-copy (string-copy path 0 j)))
                      (string-copy path i last))
              (loop j))))))))

(define (path-split-extension path)
  (let loop ((i (string-length path)))
    (if (zero? i)
      (cons path #f)
      (let* ((j (- i 1))
             (c (string-ref path j)))
        (cond
          ((and (char=? c #\.) (> j 0) (not (path-separator? (string-ref path (- j 1)))))
           (cons (string-copy path 0 i) (string-copy path i)))
          ((path-separator? c)
           (cons path #f))
          (else
           (loop j)))))))

(define (path-join% arg)
  (cond
    ((or (char? arg)
         (number? arg))
     (display arg))
    ((string? arg)
     (display (path-trim-separators arg)))
    ((list? arg)
     (for-each path-join% arg))
    (else
     (error "path-join -- argument isn't convertible to a path part"))))

(define (path-join . args)
  (let ((output (open-output-string))
        (flattened-args (let flatten-head ((head (car args))
                                           (tail (cdr args)))
                          (if (list? head)
                            (cons (flatten-head (car head) (cdr head)) tail)
                            (cons head tail)))))
    (parameterize ((current-output-port output))
      (let ((head (car flattened-args)))
        (cond
          ((or (char? head)
               (number? head))
           (display head))
          ((string? head)
           (display (path-trim-trailing-separators head))))
        (for-each path-join% (cdr flattened-args))))
    (get-output-string output)))

(define (path-join-extension path extension)
  (if (zero? (string-length extension))
    path
    (if (char=? (string-ref extension 0) #\.)
      (string-append path extension)
      (string-append path #\. extension))))

(define (path-normalize path)
  path)

