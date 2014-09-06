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

(define (ac-version) "0.1")

(define (ac-prereq version)
  (when (ac-version<? (ac-version) version)
    (ac-msg-error "Wizard version " version " or higher is required!")))

(define *option-processors* '())
(define *feature-processors* '())
(define *package-processors* '())
(define *configuration-help* '())
(define *base-output-help* '())
(define *sub-output-help* '())
(define *environment-help* '())

(define ac-arg-enable
  (case-lambda
    ((feature help-string)
     (ac-arg-enable feature help-string (lambda (v) #f) (lambda () #f)))
    ((feature help-string action-if-given)
     (ac-arg-enable feature help-string action-if-given (lambda () #f)))
    ((feature help-string action-if-given action-if-not-given)
     (set!
       *feature-processors*
       (cons
         (list feature help-string action-if-given action-if-not-given)
         *feature-processors*)))))

(define ac-arg-with
  (case-lambda
    ((package help-string)
     (ac-arg-with package help-string (lambda (v) #f) (lambda () #f)))
    ((package help-string action-if-given)
     (ac-arg-with package help-string action-if-given (lambda () #f)))
    ((package help-string action-if-given action-if-not-given)
     (set!
       *package-processors*
       (cons 
         (list package help-string action-if-given action-if-not-given)
         *package-processors*)))))

(define (ac-arg-var name help-string)
  (set! *environment-help* (cons (cons name help-string) *environment-help*)))

(define (arg-register-option canonical-name option-names required-arg? optional-arg? help-details)
  (set!
    *option-processors*
    (cons
      (option option-names required-arg? optional-arg? 
              (lambda (option name arg features packages options variables)
                (hash-table-set! options canonical-name (or arg #t))
                (values features packages options variables)))
      *option-processors*))
  (cons canonical-name help-details))

(define (arg-io-variable-processor option name arg features packages options variables)
  (hash-table-set! variables (string->symbol name) arg)
  (values features packages options variables))

(define (arg-register-io-variable name default help-arg-name help-details)
  (set!
    *option-processors*
    (cons
      (option `(,(symbol->string name)) #t #f arg-io-variable-processor)
      *option-processors*))
  (cons name help-details))

(define (arg-register-options)
  (let-syntax ((register (syntax-rules()
                           ((_ (proc alist) options ...)
                            (for-each
                              (lambda (a)
                                (set! alist (cons (apply proc a) alist)))
                              `(,options ...))))))
    (register (arg-register-option *configuration-help*)
      '(help             (#\h "help") #f #t             "display this help and exit")
      '(version          (#\V "version") #f #t          "display version information and exit")
      '(quiet            (#\q "quiet" "silent") #f #t   "do not print `[ Configure ]' messages")
      '(no-create        (#\n "no-create") #f #f        "do not create output files")
      '(no-colors        ("no-colors") #f #f            "disable colored output to terminal")
      '(config-cache     (#\C "config-cache") #f #f     "alias for `--cache-file=config.cache'"))
    (register (arg-register-io-variable *configuration-help*)
      '(cache-file       #f                       "FILE" "cache test results in FILE")
      '(srcdir           #f                        "DIR" "find the sources in DIR [configure dir or `..']"))
    (register (arg-register-io-variable *base-output-help*)
      '(prefix          "/usr/local"            "PREFIX" "install architecture-independent files in PREFIX")
      '(exec-prefix     "${prefix}"            "EPREFIX" "install architecture-dependent files in EPREFIX"))
    (register (arg-register-io-variable *sub-output-help*)
      '(bindir          "${eprefix}/bin"           "DIR" "user executables")
      '(sbindir         "${eprefix}/sbin"          "DIR" "system admin executables")
      '(libexecdir      "${eprefix}/libexec"       "DIR" "program executables")
      '(sysconfdir      "${prefix}/etc"            "DIR" "read-only single-machine data")
      '(sharedstatedir  "${prefix}/com"            "DIR" "modifiable architecture-independent data")
      '(localstatedir   "${prefix}/var"            "DIR" "modifiable single-machine data")
      '(libdir          "${prefix}/lib"            "DIR" "object code libraries")
      '(includedir      "${prefix}/include"        "DIR" "source header files")
      '(datarootdir     "${prefix}/share"          "DIR" "read-only arch.-independent data root")
      '(datadir         "${datarootdir}"           "DIR" "read-only architecture-independent data")
      '(infodir         "${datarootdir}/info"      "DIR" "info documentation")
      '(localedir       "${datarootdir}/locale"    "DIR" "locale-dependent data")
      '(mandir          "${datarootdir}/man"       "DIR" "man documentation")
      '(docdir          "${datarootdir}/doc"       "DIR" "documentation root")
      '(htmldir         "${docdir}"                "DIR" "html documentation")
      '(dvidir          "${docdir}"                "DIR" "dvi documentation")
      '(pdfdir          "${docdir}"                "DIR" "pdf documentation")
      '(psdir           "${docdir}"                "DIR" "ps documentation"))))

(define (print-usage-and-exit)
    (ac-echo-n
      :bold "Synopsis" :normal ": Wizard -- Automatic Software Source Package Configuration\n"
      :bold "Usage"    :normal ":    ./" (car (command-line)) " [options] [variables]\n\n"
      :bold "Configuration Options" :normal ":\n")
    (ac-exit))

(define ac-init
  (case-lambda
    ((package version)
     (ac-init package version "" "" ""))
    ((package version bug-report)
     (ac-init package version bug-report "" ""))
    ((package version bug-report tarname)
     (ac-init package version bug-report tarname ""))
    ((package version bug-report tarname url)
     (call-with-values
       (lambda ()
         (args-fold
           (cdr (command-line))
           (reverse *option-processors*)
           (lambda (option name arg features packages options variables)
             (ac-msg-error "Unrecognized option: " name))
           (lambda (variable features packages options variables)
             (hash-table-set! variables variable #t)
             (values features packages options variables))
           (make-hash-table)
           (make-hash-table)
           (make-hash-table)
           (make-hash-table)))
       (lambda (features packages options variables)
         (current-package (make-package features packages options variables))))
     (when (output-port? (ac-message-log-port)) (close-output-port (ac-message-log-port)))
     (ac-message-log-port (open-output-file "config.log"))
     (when (hash-table-ref/default (package-options (current-package)) 'help #f)
       (print-usage-and-exit))
     (ac-subst 'PACKAGE_NAME package)
     (ac-subst 'PACKAGE_VERSION version))))

(define (ac-output . files)
  (ac-exit))

#|;; ac-check-pkg-config works like PKG_PROG_PKG_CONFIG
(define (ac-check-pkg-config :optional (min-version "0.9.0"))
  (ac-path-prog 'PKG_CONFIG "pkg-config")
  (if (ac-have-subst? 'PKG_CONFIG)
    (let ((proc (run-process `(,(ac-subst-ref 'PKG_CONFIG) "--version") :redirects '((>& 2 1) (> 1 out)))))
      (let ((version (read-line (process-output proc 'out))))
        (ac-msg-checking "pkg-config is at least version ~a " min-version)
        (if (version<=? min-version version)
          (ac-msg-result "yes")
          (begin (ac-msg-result "no") (ac-subst 'PKG_CONFIG #f))))
      (process-wait proc))))
|#
