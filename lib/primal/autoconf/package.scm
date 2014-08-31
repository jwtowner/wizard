;;
;; Primal Autoconf
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

(define-record-type <package>
  (make-package features packages options variables)
  package?
  (features package-features)
  (packages package-packages)
  (options package-options)
  (variables package-variables))

(define current-package
  (let ((package #f))
    (case-lambda
      (()      (or package (error "ac-init was not called to initialize package")))
      ((value) (set! package value)))))

(define ac-feature
  (case-lambda
    ((feature)
     (ac-feature feature "yes"))
    ((feature value)
     (hash-table-set! (package-features (current-package)) feature value))))

(define ac-feature-ref
  (case-lambda
    ((feature)
     (hash-table-ref (package-features (current-package)) feature))
    ((feature default)
     (hash-table-ref/default (package-features (current-package)) feature))))

(define (ac-feature-exists? feature)
  (hash-table-exists? (package-features (current-package)) feature))

(define ac-package
  (case-lambda
    ((package)
     (ac-package package "yes"))
    ((package value)
     (hash-table-set! (package-packages (current-package)) package value))))

(define ac-package-ref
  (case-lambda
    ((package)
     (hash-table-ref (package-packages (current-package)) package))
    ((package default)
     (hash-table-ref/default (package-packages (current-package)) package))))

(define (ac-package-exists? package)
  (hash-table-exists? (package-packages (current-package)) package))

(define (ac-subst symbol value)
  (hash-table-set! (package-variables (current-package)) symbol value))

(define ac-subst-ref
  (case-lambda
    ((symbol)
     (hash-table-ref (package-variables (current-package)) symbol))
    ((symbol default)
     (hash-table-ref/default (package-variables (current-package)) symbol default))))

(define (ac-subst-exists? symbol)
  (hash-table-exists? (package-variables (current-package)) symbol))

