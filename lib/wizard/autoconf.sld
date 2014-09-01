(define-library (wizard autoconf)
  (export ac-version ac-prereq ac-arg-enable ac-arg-with ac-arg-var ac-init ac-output
          ac-msg-checking ac-msg-result ac-msg-notice ac-msg-warn ac-msg-error ac-msg-failure
          ac-feature ac-feature-exists? ac-feature-ref
          ac-package ac-package-exists? ac-package-ref
          ac-subst ac-subst-exists? ac-subst-ref
          ac-exit ac-message-port ac-message-log-port ac-original-input-port
          as-echo as-echo-n
          ac-version-compare ac-version=? ac-version<? ac-version>?
          ac-version<=? ac-version>=?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (except (scheme process-context) exit)
          (srfi 37)
          (srfi 69)
          (rename (wizard base) (init wizard:init))
          (prefix (except (wizard base) init) ac-))
  (include "autoconf/message.scm")
  (include "autoconf/package.scm")
  (include "autoconf.scm")
  (begin
    (wizard:init)
    (arg-register-options)))

