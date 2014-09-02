(define-library (wizard autoconf)
  (export ac-version ac-prereq ac-arg-enable ac-arg-with ac-arg-var ac-init ac-output ac-exit
          ac-feature ac-feature-exists? ac-feature-ref
          ac-package ac-package-exists? ac-package-ref
          ac-subst ac-subst-exists? ac-subst-ref
          ac-msg-checking ac-msg-result ac-msg-protip ac-msg-notice
          ac-msg-warn ac-msg-error ac-msg-failure ac-echo ac-echo-n
          ac-message-port ac-message-log-port ac-original-input-port
          ac-version-compare ac-version=? ac-version<? ac-version>?
          ac-version<=? ac-version>=?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme write)
          (except (scheme process-context) exit)
          (rename (scheme process-context) (exit ac-exit))
          (srfi 37)
          (srfi 69)
          (rename (wizard base)
            (display-message ac-echo-n)
            (message-port ac-message-port)
            (message-log-port ac-message-log-port)
            (original-input-port ac-original-input-port)
            (version-compare ac-version-compare)
            (version=? ac-version=?)
            (version<? ac-version<?)
            (version>? ac-version>?)
            (version<=? ac-version<=?)
            (version>=? ac-version>=?)))
  (include "autoconf/message.scm")
  (include "autoconf/package.scm")
  (include "autoconf.scm")
  (begin
    (arg-register-options)))

