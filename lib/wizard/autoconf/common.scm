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
;; SPECIAL,i EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(define as-exit
  (case-lambda
    (()
     (as-exit 0))
    ((obj)
     (cond-expand
        ((library (ncurses))
           (endwin)))
     (exit obj))))

(define as-message-port (make-parameter #f))
(define as-message-log-port (make-parameter #f))
(define as-original-input-port (make-parameter (current-input-port)))

(define (with-output-to-message-port thunk)
  (let ((port (as-message-port)))
    (if (output-port? port)
      (parameterize ((current-output-port port))
        (thunk))
      (thunk)))
  (let ((port (as-message-log-port)))
    (when (output-port? port)
      (parameterize ((current-output-port port))
        (thunk)))))

(define (as-echo-n . args)
  (with-output-to-message-port (lambda () (for-each display args))))

(define (as-echo . args)
  (apply as-echo-n `(,@args "\n")))

(define (ac-msg-checking . args)
  (apply as-echo-n `("[ Configure ] checking " ,@args " ... ")))

(define (ac-msg-result . args)
  (apply as-echo args))

(define (ac-msg-notice . args)
  (apply as-echo `("[ Configure ] " ,@args)))

(define (ac-msg-wiztip . args)
  (apply as-echo `("[  Wiz-Tip  ] " ,@args)))

(define (ac-msg-warn . args)
  (apply as-echo `("[  Warning  ] " ,@args)))

(define (ac-msg-error . args)
  (apply as-echo `("[   Error   ] " ,@args))
  (as-exit -1))

(define (ac-msg-failure . args)
  (apply ac-msg-error `(,@args "\n[  Bro-Tip  ] See `config.log' for more details...")))

(define (version-compare version-a version-b)
  (define (make-next-version-part s)
    (let ((last 0)
          (s-length (string-length s)))
      (lambda ()
        (let loop ((first last)
                   (i last))
          (cond
            ((> i s-length)
             '(0 . #t))
            ((or (= i s-length)
                 (char=? (string-ref s i) #\.))
             (set! last (+ i 1))
             `(,(string->number (string-copy s first i)) . #f))
            (else
             (loop first (+ i 1))))))))
  (let ((next-version-part-a (make-next-version-part version-a))
        (next-version-part-b (make-next-version-part version-b)))
    (let loop ((a (next-version-part-a))
               (b (next-version-part-b)))
      (if (and (cdr a) (cdr b))
        0
        (let ((d (- (car a) (car b))))
          (if (not (= d 0))
            d
            (loop (next-version-part-a)
                  (next-version-part-b))))))))

(define-syntax as-version-compare
  (syntax-rules ()
    ((_ version1 version2)
     (version-compare version1 version2))
    ((_ version1 version2 action-if-less)
     (when (< (version-compare version1 version2) 0)
       action-if-less))
    ((_ version1 version2 action-if-less action-if-greater-equal)
     (if (< (version-compare version1 version2) 0)
       action-if-less
       action-if-greater-equal))
    ((_ version1 version2 action-if-less action-if-equal action-if-greater)
     (let ((ordinal (version-compare version1 version2)))
       (cond
         ((< ordinal 0) action-if-less)
         ((> ordinal 0) action-if-greater)
         (else action-if-equal))))))

(define (as-version=? version1 version2) (= (version-compare version1 version2) 0))
(define (as-version<? version1 version2) (< (version-compare version1 version2) 0))
(define (as-version>? version1 version2) (> (version-compare version1 version2) 0))
(define (as-version<=? version1 version2) (<= (version-compare version1 version2) 0))
(define (as-version>=? version1 version2) (>= (version-compare version1 version2) 0))

; (define as-executeable-p)
; (define as-mkdir-p )
; (define as-tmpdir (case-lambda ))

