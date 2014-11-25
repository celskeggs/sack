#lang racket

(require racket/stxparam)
(require "utilities.rkt")
(require "boxdag.rkt")
(require "boxdag-rules.rkt")

; Currently assuming register-based.

; arguments: ((name predicate) ...)
(define (convert-behavior-expr arguments behavior)
  (if (symbol? behavior)
      (if (assoc behavior arguments)
          (if (eq? (second (assoc behavior arguments)) const?)
              (list 'const behavior)
              (error "Uncertain how to handle raw argument:" behavior))
          (error "Uncertain how to handle raw non-argument symbol:" behavior))
      (case (car behavior)
        ('get-reg
         (assert (= (length behavior) 2) "get-reg expects one argument")
         (assert (symbol? (second behavior)) "get-reg expects a symbol argument")
         (second behavior))
        (else
         (cons (car behavior) (map (curry convert-behavior-expr arguments) (cdr behavior)))))))

(define (convert-behavior-line arguments behavior)
  (case (car behavior)
    ('set-reg
     (assert (= (length behavior) 3) "set-reg expects two arguments")
     (convert-behavior-expr arguments (third behavior)))
    (else (error "Unexpected behavior type" (car behavior)))))

(define (convert-behavior name arguments behavior)
  (let ((conv (convert-behavior-line arguments behavior)))
    (define (used-in-conversion arg-pair)
      (let ((arg (car arg-pair)))
        (define (used-in-conversion-iter area)
          (or (eq? area arg)
              (and (pair? area)
                   (or (used-in-conversion-iter (car area))
                       (used-in-conversion-iter (cdr area))))))
        (used-in-conversion-iter conv)))
    (let ((used-arguments (filter used-in-conversion arguments)))
      (boxdag-rule
       (map (lambda (x) (assert (= (length x) 2) "Bad argument declaration") (cons (first x) (second x))) used-arguments)
       conv
       (cons name (map car used-arguments))
       ))))

(struct instruction-struct
  (name arguments string-gen behavior rule) #:inspector #f)
(struct mutable-platform-struct
  (name registers instrs rules) #:mutable #:inspector #f)
(struct platform-struct
  (name registers instrs rules) #:inspector #f)
(define (finalize-platform x) ; TODO
  (platform-struct (mutable-platform-struct-name x)
                   (mutable-platform-struct-registers x)
                   (reverse (mutable-platform-struct-instrs x))
                   (reverse (mutable-platform-struct-rules x))))

(define (make-instruction name args string-gen behavior)
  (instruction-struct name args string-gen behavior
                      (convert-behavior name args behavior)))
(define (add-platform-rule! platform-ref rule)
  (set-mutable-platform-struct-rules! platform-ref (cons rule (mutable-platform-struct-rules platform-ref))))
(define (add-platform-instr! platform-ref instr)
  (set-mutable-platform-struct-instrs! platform-ref (cons instr (mutable-platform-struct-instrs platform-ref))))

(define-syntax-parameter active-platform-ref
  (lambda (stx)
    (raise-syntax-error #f "use of active-platform-ref outside a platform declaration")))
(define-syntax-parameter register?
  (lambda (stx)
    (raise-syntax-error #f "use of register? outside a platform declaration")))
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
    (set! register? (lambda (r) (and (member r registers) #t)))
    (set-mutable-platform-struct-registers! active-platform-ref registers)))
(define-syntax-rule (argument-behavior argname contents)
  (add-platform-rule! active-platform-ref (boxdag-rule (list (cons 'argname any?))
                                                       '(arg argname)
                                                       'contents)))
(define (is-nontrivial-rule rule)
  (not (and (symbol? (boxdag-rule-find rule))
            (= (length (boxdag-rule-args rule)) 1)
            (equal? (cdar (boxdag-rule-args rule)) any?))))
(define-syntax-rule (instructions instr ...)
  (begin
    (begin
      (let ((i-struct (instruction . instr)))
        (add-platform-instr! active-platform-ref i-struct)
        (if (is-nontrivial-rule (instruction-struct-rule i-struct))
            (add-platform-rule! active-platform-ref (instruction-struct-rule i-struct))
            (void)))) ...))
(define-syntax-rule (use-standard-reductions)
  (begin
   (reduction-calc (+ (a const?) (b const?)) (wrap-const (+ a b)))
   (reduction-calc (* (a const?) (b const?)) (wrap-const (* a b)))
   ))
(define-syntax-rule (platform name entry ...)
  (define name (let ((platform-def (mutable-platform-struct 'name (void) empty empty))
                     (is-register? (lambda (x) (error "No (register-based) declaration!"))))
                 (syntax-parameterize ([active-platform-ref (make-rename-transformer #'platform-def)]
                                       [register? (make-rename-transformer #'is-register?)])
                                      entry ...)
                 (finalize-platform platform-def))))

(define (platform-process platform input)
  (let ((boxdag (make-boxdag input)))
    (apply-boxdag-rules-all (platform-struct-rules platform) boxdag)
    boxdag))
(define (calc-fixup-arg arg cond)
  (if (eq? cond const?)
      (list 'const arg)
      arg))
(define (calc-fixup-recurse args data)
  (cond ((pair? data)
         (cons (calc-fixup-recurse args (car data))
               (calc-fixup-recurse args (cdr data))))
        ((and (assoc data args) (eq? (cdr (assoc data args)) const?))
         (list 'const data))
        (else data)))
(define-syntax-rule (reduction-advanced (arg cond) ... find repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...) (calc-fixup-recurse (list (cons 'arg cond) ...) 'find) 'repl)))
(define-syntax-rule (reduction-simple (name (arg cond) ...) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   'repl)))
(define-syntax-rule (reduction-calc (name (arg cond) ...) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   (lambda (vars)
                                     (let ((arg (cdr (assoc 'arg vars))) ...)
                                       repl)))))

(define (any? x)
  #t)
(define (const? x)
  (integer? x))
(define (wrap-const x)
  (list 'const x))

(platform x86
          (register-based eax ebx ecx edx esi edi)
          (argument-behavior argid (get-memory (+ (get-reg ebp) (+ (const 8) (* (const argid) (const 4))))))
          (use-standard-reductions)
          (reduction-simple (get-memory (c const?))
                            (x86/get-memory/c c))
          (reduction-advanced (a any?) (b const?)
                              (get-memory (+ a b))
                              (x86/get-memory/dc a b))
          (reduction-simple (get-memory (a any?))
                            (x86/get-memory/d a))
          (instructions
           [(x86/movfm/c (dest any?) (source const?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (x86/get-memory/c source))]
           
           [(x86/movfm/d (dest any?) (source any?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (x86/get-memory/d (get-reg source)))]
           
           [(x86/add/dc (dest any?) (source const?))
            ("  add " dest ", " source)
            (set-reg dest (+ (get-reg dest) source))]
           
           [(x86/mov/d (dest any?) (source any?))
            ("  mov " dest ", " source)
            (set-reg dest (get-reg source))] ; should yield a nullary rule.
           
           ;[(x86/mov/c (dest any?) (source const?))
           ; ("  mov " dest ", " source)
           ; (set-reg dest source)]
           ))

(strip-boxes (get-boxdag-content (platform-process x86 '(+ (arg 0) (const 10)))))
