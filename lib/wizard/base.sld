(define-library (wizard base)
  
  (export version version-compare* version-compare
          version=? version<? version>? version<=? version>=?

          make-hash-table hash-table? hash-table-contains? hash-table-empty?
          hash-table-ref hash-table-ref/default hash-table-set!
          hash-table-delete! hash-table-update! hash-table-update!/default
          hash-table-copy alist->hash-table hash-table->alist

          original-input-port message-port message-log-port
          color-output? color-terminal-supported? color-terminal-port?
          define-display-syntax display-message display-message*
          display-message-transformer% make-ansi-tty-command
          ansi-tty-command ansi-tty-command? ansi-tty-command->string
 
          path-normalize)

  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (scheme process-context))

  ; hash-table (R7RS-large compatible subset)

  (cond-expand
    (gauche
      (import
        (only (gauche base)
          make-hash-table hash-table? hash-table-get hash-table-put! hash-table-exists?
          hash-table-delete! hash-table-copy alist->hash-table hash-table->alist)
        (prefix (gauche base) gauche:))
      (include "base/hash-table-gauche.scm"))
    (else
      (import
        (except (srfi 69)
          hash-table-exists?)
        (rename (srfi 69)
          (hash-table-exists? hash-table-contains?)))
      (include "base/hash-table-srfi-69.scm")))

  ; terminal-port?

  (cond-expand
    (chibi
      (import (rename (chibi filesystem) (is-a-tty? terminal-port?))))
    (chicken
      (import (only (posix) terminal-port?)))
    (gauche
      (import (rename (gauche base) (sys-isatty terminal-port?))))
    (else
      (begin (define (terminal-port? port) #f))))

  ; file paths 

  (cond-expand
    (chibi
      (import
        (only (chibi pathname)
          path-normalize)))
    (chicken
      (import
        (rename (files)
          (normalize-pathname path-normalize))))
    (gauche
      (import
        (only (file util)
          expand-path
          simplify-path))
      (begin
        (define (path-normalize path)
          (simplify-path (expand-path path)))))
    (else
      (error "implementation not supported")))

  (include "base.scm"))

