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

(define-record-type <bundle>
  (make-bundle definitions features packages options variables)
  bundle?
  (definitions bundle-definitions)
  (features bundle-features)
  (options bundle-options)
  (packages bundle-packages)
  (variables bundle-variables))

(define current-bundle
  (let ((bundle #f))
    (case-lambda
      (()      (or bundle (error "ac-init was not called to initialize configuration bundle")))
      ((value) (set! bundle value)))))

(define ac-define
  (case-lambda
    ((variable)
     (ac-define variable 1 ""))
    ((variable value)
     (ac-define variable value ""))
    ((variable value description)
     (hash-table-set! (bundle-definitions (current-bundle)) variable (cons value description)))))

(define ac-define-ref
  (case-lambda
    ((variable)
     (hash-table-ref (bundle-definitions (current-bundle)) variable))
    ((variable default)
     (hash-table-ref/default (bundle-definitions (current-bundle)) variable default))))

(define (ac-define-exists? variable)
  (hash-table-exists? (bundle-definitions (current-bundle)) variable))

(define ac-feature
  (case-lambda
    ((feature)
     (ac-feature feature "yes"))
    ((feature value)
     (hash-table-set! (bundle-features (current-bundle)) feature value))))

(define ac-feature-ref
  (case-lambda
    ((feature)
     (hash-table-ref (bundle-features (current-bundle)) feature))
    ((feature default)
     (hash-table-ref/default (bundle-features (current-bundle)) feature))))

(define (ac-feature-exists? feature)
  (hash-table-exists? (bundle-features (current-bundle)) feature))

(define ac-package
  (case-lambda
    ((package)
     (ac-package package "yes"))
    ((package value)
     (hash-table-set! (bundle-packages (current-bundle)) package value))))

(define ac-package-ref
  (case-lambda
    ((package)
     (hash-table-ref (bundle-packages (current-bundle)) package))
    ((package default)
     (hash-table-ref/default (bundle-packages (current-bundle)) package))))

(define (ac-package-exists? package)
  (hash-table-exists? (bundle-packages (current-bundle)) package))

(define (ac-subst variable value)
  (hash-table-set! (bundle-variables (current-bundle)) variable value))

(define ac-subst-ref
  (case-lambda
    ((variable)
     (hash-table-ref (bundle-variables (current-bundle)) variable))
    ((variable default)
     (hash-table-ref/default (bundle-variables (current-bundle)) variable default))))

(define (ac-subst-exists? variable)
  (hash-table-exists? (bundle-variables (current-bundle)) variable))

