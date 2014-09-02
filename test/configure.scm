(import (scheme base)
        (scheme write)
        (wizard autoconf))

(ac-prereq "0.1")
(ac-init "Test Sofware" "1.0" "test@test.com")

(ac-msg-notice "Welcome to " :bold "Wizard" :normal ", an automatic software package configuration tool" :italic "!")
(ac-msg-notice "Now proceeding with the automated system test ...")
(ac-msg-checking "libc")
(ac-msg-result :yes)
(ac-msg-checking "non-existent library")
(ac-msg-result :no)
(ac-msg-protip :italic "this is a protip")
(ac-msg-warn "this is a warning" :italic "!")
(ac-msg-failure "something " :underline "terrible" :normal " happened" :italic "!")

(ac-output)
