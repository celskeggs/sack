#lang racket

(require racket/stxparam)
(require "utilities.rkt")
(require "boxdag-rules.rkt")

; Currently assuming register-based.

; arguments: ((name predicate) ...)
(define (convert-behavior-expr arguments behavior)
  (case (car behavior)
    ('get-reg
     (assert (= (length behavior) 2) "get-reg expects one argument")
     (assert (symbol? (second behavior)) "get-reg expects a symbol argument")
     (second behavior))
    (else
     (cons (car behavior) (map (curry convert-behavior-expr arguments) (cdr behavior))))))

(define (convert-behavior-line arguments behavior)
  (case (car behavior)
    ('set-reg
     (assert (= (length behavior) 3) "set-reg expects two arguments")
     (convert-behavior-expr arguments (third behavior)))
    (else (error "Unexpected behavior type" (car behavior)))))

(define (convert-behavior name arguments behavior)
  (boxdag-rule
   (map (lambda (x) (assert (= (length x) 2) "Bad argument declaration") (cons (first x) (second x))) arguments)
   (convert-behavior-line arguments behavior)
   (list name (map car arguments))
   ))

(struct instruction-struct
  (name arguments string-gen behavior rule) #:inspector #f)
(struct mutable-platform-struct
  (name registers) #:mutable #:inspector #f)
(define (finalize-platform x) ; TODO
  x)

(define (make-instruction name args string-gen behavior)
  (instruction-struct name args string-gen behavior
                      (convert-behavior name args behavior)))

(define-syntax-parameter active-platform-ref
  (lambda (stx)
    (raise-syntax-error #f "use of active-platform-ref outside a platform declaration")))
(define-syntax-rule (instruction (name (arg-name arg-type) ...)
                                 (string-part ...)
                                 instr-behavior)
  (make-instruction 'name
                    (list (list 'arg-name arg-type) ...)
                    (lambda (arg-name ...)
                      (list string-part ...))
                    'instr-behavior))
(define-syntax-rule (register-based reg ...)
  (begin
    (define registers '(reg ...))
    (define (register? r) (and (member r registers) #t))
    (set-mutable-platform-struct-registers! active-platform-ref registers)))
(define-syntax-rule (platform name entry ...)
  (define name (let ((platform-def (mutable-platform-struct 'name (void))))
                 (syntax-parameterize ([active-platform-ref (make-rename-transformer #'platform-def)])
                                      entry ...)
                 (finalize-platform platform-def))))

(define (register? x)
  (member x '(eax ebx ecx edx)))
(define sample (instruction (x86/add/dc (dest register?) (source register?))
                            ("  add " dest ", " source)
                            (set-reg dest (+ (get-reg dest) (get-reg source)))))

(platform x86
          (register-based eax ebx ecx edx esi edi)
          )
