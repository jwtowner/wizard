(define-library (wizard autoconf)
  (export version prereq arg-enable arg-with arg-var init output exit
          version-compare version=? version<? version>? version<=? version>=?
          cache-variable cache-variable-exists? cache-variable-ref
          config-define config-define-exists? config-define-ref
          feature feature-exists? feature-ref
          package package-exists? package-ref
          subst subst-exists? subst-ref
          msg-checking msg-result msg-protip msg-notice
          msg-warn msg-error msg-failure echo echo-n
          message-port message-log-port original-input-port
          check-file* check-file check-files)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (scheme process-context)
          (srfi 37)
          (wizard base)
          (rename (wizard base)
            (display-message echo-n)))
  (include "autoconf/bundle.scm")
  (include "autoconf/message.scm")
  (include "autoconf/check.scm")
  (include "autoconf.scm")
  (begin
    (register-options)))

