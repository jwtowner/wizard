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

(define (check-file* path)
  (unless (string? path)
    (error "check-file -- file path must be a string"))
  (msg-checking "for file \"" path "\"")
  (let* ((normpath (path-normalize path))
         (key (string-append "file:" normpath))
         (cache (bundle-cache (current-bundle)))
         (cached #t)
         (result (hash-table-ref cache key
                   (lambda ()
                     (set! cached #f)
                     (let ((x (file-exists? normpath)))
                       (hash-table-set! cache key x)
                       x)))))
      (if result
        (echo-n :bold :green "yes")
        (echo-n :bold :red "no"))
      (if cached
        (echo-n " " :green "(cached)\n")
        (echo-n "\n"))))

(define-syntax check-file
  (syntax-rules ()
    ((_ path)
     (check-file path #t #f))
    ((_ path action-if-exists)
     (check-file path action-if-exist #f))
    ((_ path action-if-exists action-if-not-found)
     (if (check-file* path)
       action-if-exists
       action-if-not-found))))

(define-syntax check-files
  (syntax-rules ()
    ((_ path)
     (check-files paths #t #f))
    ((_ paths action-if-exists)
     (check-files paths action-if-exist #f))
    ((_ paths action-if-exists action-if-not-found)
     (for-each (lambda (path) (check-file path action-if-exists action-if-not-found)) paths))))

#|;; check-pkg-config works like PKG_PROG_PKG_CONFIG
(define (check-pkg-config :optional (min-version "0.9.0"))
  (path-prog 'PKG_CONFIG "pkg-config")
  (if (have-subst? 'PKG_CONFIG)
    (let ((proc (run-process `(,(subst-ref 'PKG_CONFIG) "--version") :redirects '((>& 2 1) (> 1 out)))))
      (let ((version (read-line (process-output proc 'out))))
        (msg-checking "pkg-config is at least version ~a " min-version)
        (if (version<=? min-version version)
          (msg-result "yes")
          (begin (msg-result "no") (subst 'PKG_CONFIG #f))))
      (process-wait proc))))
|#

