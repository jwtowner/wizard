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

(define original-input-port (make-parameter (current-input-port)))
(define original-output-port (make-parameter (current-output-port)))
(define message-port (make-parameter #f))
(define message-log-port (make-parameter #f))

(define (default-message-ports)
  (let ((msg-port (message-port))
        (msg-log-port (message-log-port)))
    (set! msg-port (or (output-port? msg-port) msg-port (current-output-port)))
    (if (output-port? msg-log-port)
      (list msg-port msg-log-port)
      (list msg-port))))

(define color-output? (make-parameter #t))

(define *color-terminal-supported*
  (let-syntax ((color-term? (syntax-rules () ((_ t names ...) (or (string=? t names) ...)))))
    (let ((term (get-environment-variable "TERM")))
      (if (string? term)
          (color-term? term "xterm" "xterm-16color" "xterm-88color" "xterm-256color"
                            "rxvt" "rxvt-16color" "konsole" "konsole-16color")
          #f))))

(define (color-terminal-supported?)
  *color-terminal-supported*)

(define (color-terminal-port? port)
  (and *color-terminal-supported*
       (color-output?)
       (terminal-port? port)))

(define-syntax define-display-syntax
  (syntax-rules ()
    ((_ (name transformer-name) ((display-proc make-command-token) (command-name command-data) ...))
     (begin
       (define-syntax transformer-name
         (syntax-rules (command-name ...)
           ((transformer-name (args (... ...)) ())
            (display-proc args (... ...)))
           ((transformer-name (args (... ...)) (commands (... ...)) command-name expr (... ...))
            (transformer-name (args (... ...)) (commands (... ...) command-data) expr (... ...))) ...
           ((transformer-name (args (... ...)) () x expr (... ...))
            (transformer-name (args (... ...) x) () expr (... ...)))
           ((transformer-name (args (... ...)) (commands (... ...)) x expr (... ...))
            (transformer-name (args (... ...) (make-command-token commands (... ...))) () x expr (... ...)))))
       (define-syntax name
         (syntax-rules ()
           ((name expressions (... ...))
            (transformer-name () () expressions (... ...)))))))))

(define-record-type <ansi-tty-code>
  (make-ansi-tty-code code)
  ansi-tty-code?
  (code ansi-tty-code->string))

(define-syntax make-ansi-tty-command%
  (syntax-rules ()
    ((_ commands ...)
     (make-ansi-tty-code (string-append "\x1B[0" commands ... "m")))))

(define (display-message* port-or-token . tokens)
  (let loop ((ports (cond
                      ((output-port? port-or-token)
                       (list port-or-token))
                      (else
                       (set! tokens (cons port-or-token tokens))
                       (default-message-ports)))))
    (unless (null? ports)
      (let ((p (car ports)))
        (cond
          ((color-terminal-port? p)
            (for-each
              (lambda (t)
                (if (ansi-tty-code? t)
                  (write-string (ansi-tty-code->string t) p)
                  (display t p)))
              tokens)
            (write-string "\x1B[0m" p))
          (else
            (for-each
              (lambda (t)
                (unless (ansi-tty-code? t)
                  (display t p)))
              tokens))))
      (loop (cdr ports)))))

(define-display-syntax (display-message display-message-transformer%)
  ((display-message* make-ansi-tty-command%)
    (:normal ";0")
    (:bold ";1")
    (:faint ";2")
    (:italic ";3")
    (:underline ";4")
    (:blink ";5")
    (:strikethrough ";9")
    (:black ";30")
    (:red ";31")
    (:green ";32")
    (:yellow ";33")
    (:blue ";34")
    (:magenta ";35")
    (:cyan ";36")
    (:white ";37")
    (:black-bg ";40")
    (:red-bg ";41")
    (:green-bg ";42")
    (:yellow-bg ";43")
    (:blue-bg ";44")
    (:magenta-bg ";45")
    (:cyan-bg ";46")
    (:white-bg ";47")))

(define (version-compare* version-a version-b)
  (define (make-next-version-part s)
    (let ((last 0)
          (s-length (string-length s)))
      (lambda ()
        (let loop ((first last)
                   (i last))
          (cond
            ((> i s-length)
             '(0 . #t))
            ((or (= i s-length)
                 (char=? (string-ref s i) #\.))
             (set! last (+ i 1))
             `(,(string->number (string-copy s first i)) . #f))
            (else
             (loop first (+ i 1))))))))
  (let ((next-version-part-a (make-next-version-part version-a))
        (next-version-part-b (make-next-version-part version-b)))
    (let loop ((a (next-version-part-a))
               (b (next-version-part-b)))
      (if (and (cdr a) (cdr b))
        0
        (let ((d (- (car a) (car b))))
          (if (not (= d 0))
            d
            (loop (next-version-part-a)
                  (next-version-part-b))))))))

(define-syntax version-compare
  (syntax-rules ()
    ((_ version1 version2)
     (version-compare* version1 version2))
    ((_ version1 version2 action-if-less)
     (when (< (version-compare* version1 version2) 0)
       action-if-less))
    ((_ version1 version2 action-if-less action-if-greater-equal)
     (if (< (version-compare* version1 version2) 0)
       action-if-less
       action-if-greater-equal))
    ((_ version1 version2 action-if-less action-if-equal action-if-greater)
     (let ((ordinal (version-compare* version1 version2)))
       (cond
         ((< ordinal 0) action-if-less)
         ((> ordinal 0) action-if-greater)
         (else action-if-equal))))))

(define (version=? version1 version2) (= (version-compare* version1 version2) 0))
(define (version<? version1 version2) (< (version-compare* version1 version2) 0))
(define (version>? version1 version2) (> (version-compare* version1 version2) 0))
(define (version<=? version1 version2) (<= (version-compare* version1 version2) 0))
(define (version>=? version1 version2) (>= (version-compare* version1 version2) 0))

; (define executeable?)
; (define mkdir )
; (define tmpdir (case-lambda ))

