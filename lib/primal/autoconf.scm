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

;;
;; version and prerequisite
;;

(define (ac-version) "0.1")

(define (ac-prereq version)
  (when (as-version<? (ac-version) version)
    (ac-msg-error "Primal Autoconf version " version " or higher is required")))

;;
;; messages
;;

(define (ac-msg-checking . args)
  (apply as-echo-n `("[ Configure ]\tchecking " ,@args " ... ")))

(define (ac-msg-result . args)
  (apply as-echo args))

(define (ac-msg-notice . args)
  (apply as-echo `("[ Configure ]\t" ,@args)))

(define (ac-msg-warn . args)
  (apply as-echo `("[  Warning  ]\t" ,@args)))

(define (ac-msg-error . args)
  (apply as-echo `("[   Error   ]\t" ,@args))
  (as-exit -1))

(define (ac-msg-failure . args)
  (apply ac-msg-error `(,@args "\n[   Error   ]\tSee `config.log' for more details...")))

;;
;; configuration package
;;

(define-record-type <package>
  (make-package defs substs features packages)
  package?
  (defs package-definitions)
  (substs package-substitutions)
  (features package-features)
  (packages package-packages))

(define current-package
  (let ((package #f))
    (case-lambda
      (()      (or package (error "ac-init was not called to initialize package")))
      ((value) (set! package value)))))

(define ac-define
  (case-lambda
    ((symbol)
     (ac-define symbol 1))
    ((symbol value)
     (hash-table-set! (package-definitions (current-package)) symbol value))))

(define ac-define-ref
  (case-lambda
    ((definition)
     (hash-table-ref (package-definitions (current-package)) definition))
    ((definition default)
     (hash-table-ref/default (package-definitions (current-package)) definition default))))

(define (ac-define-exists? definition)
   (hash-table-exists? (package-definitions (current-package)) definition))

(define (ac-subst symbol value)
  (hash-table-set! (package-substitutions (current-package)) symbol value))

(define ac-subst-ref
  (case-lambda
    ((symbol)
     (hash-table-ref (package-substitutions (current-package)) symbol))
    ((symbol default)
     (hash-table-ref/default (package-substitutions (current-package)) symbol default))))

(define (ac-subst-exists? symbol)
  (hash-table-exists? (package-substitutions (current-package)) symbol))

(define ac-feature
  (case-lambda
    ((feature)
     (ac-define feature "yes"))
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
     (ac-define package "yes"))
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

;;
;; initialization and command line parsing
;;

(define feature-processors '())
(define package-processors '())

(define ac-arg-feature
  (case-lambda
    ((feature help-string) (ac-arg-feature feature help-string (lambda (v) #f) (lambda () #f)))
    ((feature help-string action-if-given) (ac-arg-feature feature help-string action-if-given (lambda () #f)))
    ((feature help-string action-if-given action-if-not-given)
     (set! feature-processors (cons (list feature help-string action-if-given action-if-not-given) feature-processors)))))

(define ac-arg-with
  (case-lambda
    ((package help-string) (ac-arg-with package help-string (lambda (v) #f) (lambda () #f)))
    ((package help-string action-if-given) (ac-arg-with package help-string action-if-given (lambda () #f)))
    ((package help-string action-if-given action-if-not-given)
     (set! package-processors (cons (list package help-string action-if-given action-if-not-given) package-processors)))))

(define ac-init
  (case-lambda
    ((package version) (ac-init package version "" "" ""))
    ((package version bug-report) (ac-init package version bug-report "" ""))
    ((package version bug-report tarname) (ac-init package version bug-report tarname ""))
    ((package version bug-report tarname url)
     (when (output-port? (as-message-log-port)) (close-output-port (as-message-log-port)))
     (as-message-log-port (open-output-file "config.log"))
     (current-package (make-package (make-hash-table) (make-hash-table) (make-hash-table) (make-hash-table)))
     (ac-subst 'PACKAGE_NAME package)
     (ac-subst 'PACKAGE_VERSION version)
     )))

#|;;> cf-check-pkg-config works like PKG_PROG_PKG_CONFIG
(define (cf-check-pkg-config :optional (min-version "0.9.0"))
  (cf-path-prog 'PKG_CONFIG "pkg-config")
  (if (cf-have-subst? 'PKG_CONFIG)
    (let ((proc (run-process `(,(cf-ref 'PKG_CONFIG) --version) :redirects '((>& 2 1) (> 1 out)))))
      (let ((version (read-line (process-output proc 'out))))
        (cf-msg-checking "pkg-config is at least version ~a " min-version)
        (if (version<=? min-version version)
          (cf-msg-result "yes")
          (begin (cf-msg-result "no") (cf-subst 'PKG_CONFIG #f))))
      (process-wait proc))))
|#
