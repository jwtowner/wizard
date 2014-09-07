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

(define-syntax echo
  (syntax-rules ()
    ((_ args ...)
     (echo-n args ... "\n"))))

(define-syntax msg-notice
  (syntax-rules ()
    ((_ args ...)
     (echo-n "[ " :cyan   "Configure" :normal " ] " args ... "\n"))))

(define-syntax msg-protip
  (syntax-rules ()
    ((_ args ...)
     (echo-n "[ " :blue   " Pro-tip " :normal " ] " args ... "\n"))))

(define-syntax msg-warn
  (syntax-rules ()
    ((_ args ...)
     (echo-n "[ " :yellow " Warning " :normal " ] " args ... "\n"))))

(define-syntax msg-error
  (syntax-rules ()
    ((_ args ...)
     (begin
       (echo-n "[ " :red  "  Error  " :normal " ] " args ... "\n")
       (exit -1)))))

(define-syntax msg-failure
  (syntax-rules ()
    ((_ args ...)
     (begin
       (echo-n "[ " :red  "  Error  " :normal " ] " args ... "\n")
       (echo-n "[ " :red  "  Error  " :normal " ] " :bold "See `config.log' for more details...\n")
       (exit -1)))))

(define-syntax msg-checking
  (syntax-rules ()
    ((_ args ...)
     (echo-n "[ " :cyan   "Configure" :normal " ] checking " args ... " ... "))))

(define-syntax msg-result
  (syntax-rules (:yes :no)
    ((_ :yes args ...)
     (echo-n :bold :italic :green "yes" args ... "\n"))
    ((_ :no args ...)
     (echo-n :bold :italic :red "no" args ... "\n"))
    ((_ args ...)
     (echo-n args ... "\n"))))

