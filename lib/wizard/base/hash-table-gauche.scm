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

(define (hash-table-contains? ht key)
  (hash-table-exists? ht key))

(define (hash-table-size ht)
  (hash-table-num-entries ht))

(define (hash-table-empty? ht)
  (= (hash-table-size ht) 0))

(define hash-table-ref
  (case-lambda
    ((ht key)
     (hash-table-get ht key))
    ((ht key failure)
     (if (hash-table-contains? ht key)
       (hash-table-get ht key)
       (failure)))))

(define (hash-table-ref/default ht key default)
  (hash-table-get ht key default))

(define (hash-table-set! ht key value)
  (hash-table-put! ht key value))

(define hash-table-update!
  (case-lambda
    ((ht key)
     (gauche:hash-table-update! ht key))
    ((ht key failure)
     (if (hash-table-contains? ht key)
       (gauche:hash-table-update! ht key)
      (failure)))))

(define (hash-table-update!/default ht key default)
  (gauche:hash-table-update! ht key default))

