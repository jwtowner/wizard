(import (scheme base)
        (scheme write)
        (wizard autoconf))

(ac-prereq "0.1")
(ac-init "Test Sofware" "1.0" "test@test.com")

(as-echo "Hello, World!")
(as-echo (as-version-compare "1.0" "1.0"))

(ac-msg-notice "this is a notice!")
(ac-msg-checking "libc")
(ac-msg-result "yes")
(ac-msg-warn "this is a warning!")
(ac-msg-failure "something bad happened!")

(ac-output)
