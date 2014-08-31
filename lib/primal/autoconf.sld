(define-library (primal autoconf)
  (export ac-version ac-prereq ac-arg-enable ac-arg-with ac-arg-var ac-init
          ac-msg-checking ac-msg-result ac-msg-notice ac-msg-warn ac-msg-error ac-msg-failure
          ac-feature ac-feature-exists? ac-feature-ref
          ac-package ac-package-exists? ac-package-ref
          ac-subst ac-subst-exists? ac-subst-ref
          as-echo as-echo-n as-exit as-message-port as-message-log-port
          as-original-input-port as-version-compare as-version=? as-version<?
          as-version>? as-version<=? as-version>=?)
  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme process-context)
          (scheme write)
          (srfi 37)
          (srfi 69))
  (include "autoconf/common.scm")
  (include "autoconf/package.scm")
  (include "autoconf.scm")
  (begin
    (arg-register-options)))
