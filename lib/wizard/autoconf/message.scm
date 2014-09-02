;;
;; Wizard - Automatic software source package configuration
;; Copyright (C) 2014 Jesse W. Towner
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the authors nor the names of its contributors
;; may be used to endorse or promote products derived from this
;; software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL,i EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;

(define-syntax ac-echo
  (syntax-rules ()
    ((_ args ...)
     (ac-echo-n args ... "\n"))))

(define-syntax ac-msg-notice
  (syntax-rules ()
    ((_ args ...)
     (ac-echo-n "[ " :cyan   "Configure" :normal " ] " args ... "\n"))))

(define-syntax ac-msg-protip
  (syntax-rules ()
    ((_ args ...)
     (ac-echo-n "[ " :blue   " Pro-tip " :normal " ] " args ... "\n"))))

(define-syntax ac-msg-warn
  (syntax-rules ()
    ((_ args ...)
     (ac-echo-n "[ " :yellow " Warning " :normal " ] " args ... "\n"))))

(define-syntax ac-msg-error
  (syntax-rules ()
    ((_ args ...)
     (begin
       (ac-echo-n "[ " :red  "  Error  " :normal " ] " args ...)
       (ac-exit -1)))))

(define-syntax ac-msg-failure
  (syntax-rules ()
    ((_ args ...)
     (begin
       (ac-echo-n "[ " :red  "  Error  " :normal " ] " args ... "\n")
       (ac-echo-n "[ " :red  "  Error  " :normal " ] " :bold "See `config.log' for more details...\n")
       (ac-exit -1)))))

(define-syntax ac-msg-checking
  (syntax-rules ()
    ((_ args ...)
     (ac-echo-n "[ " :cyan   "Configure" :normal " ] checking " args ... " ... "))))

(define-syntax ac-msg-result
  (syntax-rules (:yes :no)
    ((_ :yes)
     (ac-echo-n :bold :italic :green "yes\n"))
    ((_ :no)
     (ac-echo-n :bold :italic :red "no\n"))
    ((_ args ...)
     (ac-echo-n args ... "\n"))))


