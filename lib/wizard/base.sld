(define-library (wizard base)
  (export init exit
          original-input-port original-output-port
          message-port message-log-port
          as-echo as-echo-n
          version-compare* version-compare version=?
          version<? version>? version<=? version>=?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (rename (scheme process-context) (exit scheme:exit)))
  (cond-expand
    ((library (ncurses)) (import (prefix (ncurses) ncurses:))))
  (include "base.scm"))

