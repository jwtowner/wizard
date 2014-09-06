(define-library (wizard base)
  (export original-input-port original-output-port
          message-port message-log-port
          color-output? color-terminal-supported? color-terminal-port?
          define-display-syntax
          display-message display-message* display-message-transformer%
          make-ansi-tty-command ansi-tty-command ansi-tty-command? ansi-tty-command->string

          version-compare* version-compare version=?
          version<? version>? version<=? version>=?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (scheme process-context))
  (cond-expand
    (chibi
      (import (rename (chibi filesystem) (is-a-tty? terminal-port?))))
    (chicken
      (import (only (posix) terminal-port?)))
    (else
      (define (terminal-port? port) #f)))
  (include "base.scm"))

