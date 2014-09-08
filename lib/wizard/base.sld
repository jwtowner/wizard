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
 
          path-separator path-separator? path-absolute? path-relative?
          path-basename path-dirname path-extension path-split
          path-split-extension path-join path-join-extension path-normalize)

  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (scheme process-context))

  ; hashtables (R7RS-large compatible subset)

  (cond-expand
    (gauche
      (import
        (only (gauche base)
          hash-table? hash-table-delete! hash-table-copy
          alist->hash-table hash-table->alist)
        (rename (gauche base)
          (make-hash-table gauche:make-hash-table)
          (hash-table-get gauche:hash-table-get)
          (hash-table-put! gauche:hash-table-put!)
          (hash-table-exists? gauche:hash-table-exists?)))
      (include "base/hash-table-gauche.scm"))
    (sagittarius
      (import
        (only (rnrs hashtables (6))
          make-hash-table hash-table? hash-table-contains? hash-table-size
          hash-table-keys hash-table-set! hash-table-delete! hash-table-copy)
        (rename (rnrs hashtables (6))
          (hash-table-ref r6rs:hash-table-ref)
          (hash-table-update! r6rs:hash-table-update!)
          (hash-table-entries r6rs:hash-table-entries)))
      (include "base/hash-table-r6rs.scm"))
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

  (include "base/path.scm")
  (include "base.scm"))

