#lang racket

(require "utilities.rkt")
(require "boxdag.rkt")
(require "boxdag-rules.rkt")

(define sample '(math (u4 u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (arg 1))
                       (swr 2 (+ (swr 0) (swr 1)))
                       (return (swr 2)))))

; x86 information
(define x86-platform '(platform x86
                                (registers eax ebx ecx edx esi edi)
                                (return-behavior returned-value
                                                 (set-register eax returned-value))
                                (argument-behavior argnum
                                                   (get-memory (+ (get-register ebp) (+ 8 (* argnum 4)))))
                                (instruction-delimiter "\n")
                                (definition-syntax "global " name "\n" name ":")
                                (jumptarget-syntax ".lab" name ":")
                                (instructions
                                 (inst (x86/movfm/c (register dest) (constant source))
                                       ("  mov " dest ", [" source "]")
                                       (behavior
                                        (set-reg dest (x86/get-memory/c (unconst source))))
                                       (yields
                                        (rule
                                         (source const?)
                                         (x86/get-memory/c source)
                                         (x86/movfm/c source))))
                                 (inst (x86/add/dc (register dest) (register source))
                                       ("  add " dest ", " source)
                                       (behavior
                                        (set-reg dest (+ (get-reg dest) (get-reg source))))
                                       (yields
                                        (rule
                                         (source any?) (dest any?)
                                         (+ dest source)
                                         (x86/add/dc dest source))))
                                 (inst (x86/mov/d (register dest) (register source))
                                       ("  mov " dest ", " source)
                                       (behavior
                                        (set-reg dest (get-reg source)))
                                       (yields
                                        (rule
                                         (source any?)
                                         source
                                         (x86/mov/d source)
                                        ))) ; would yield a nullary rule.
                                 (inst (x86/mov/c (register dest) (constant source))
                                       ("  mov " dest ", " source)
                                       (behavior
                                        (set-reg dest (unconst source)))
                                       (yields
                                        (rule
                                         (source const?)
                                         source
                                         (x86/mov/c (unconst source)))))
                                 )))

(define-syntax-rule (boxdag-rule-advanced (arg cond) ... find repl)
  (boxdag-rule (list (cons 'arg cond) ...) 'find 'repl))
(define-syntax boxdag-rule-simple
  (syntax-rules ()
    [(boxdag-rule-simple (name (arg cond) ...) repl) (boxdag-rule (list (cons 'arg cond) ...) (list 'name 'arg ...) 'repl)]))
(define-syntax boxdag-rule-calc
  (syntax-rules ()
    [(boxdag-rule-simple (name (arg cond) ...) repl)
     (boxdag-rule (list (cons 'arg cond) ...)
                  (list 'name 'arg ...)
                  (lambda (vars)
                    (let ((arg (cdr (assoc 'arg vars))) ...)
                      repl)))]))
(begin-for-syntax
  (define (add-syntax-suffix stx name suffix)
    (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum name)) (symbol->string (syntax->datum suffix)))))))
(define-syntax boxdag-rule-variant
  (syntax-rules ()
    [(boxdag-rule-variant name (suffix-symbol (argument ...) (run-arg ...)) ...)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           [(name in out)
            #`(list
               (boxdag-rule-simple (in argument ...) (#,(add-syntax-suffix stx #'out #'suffix-symbol) run-arg ...)) ...)])))]))
(define (any? x) #t)
(define (const x) (list 'const x))
(define (const? x) (and (list? x) (= (length x) 2) (eq? (first x) 'const) (integer? (second x))))
(define (unconst x)
  (assert (const? x) "Expected a constant.")
  (second x))
(define (reg? x) (and (list? x) (= (length x) 2) (eq? (first x) 'reg)))
(define (mem? x) (and (list? x) (or (eq? (car x) 'get-memory)
                                    (eq? (car x) 'x86/get-memory/dc)
                                    (eq? (car x) 'x86/get-memory/dd))))

(define bd (make-boxdag '(+ (arg 0) (const 10))))
(boxdag-rule-variant x86/2op-comu
                     (/dc ((a any?) (b const?)) (a (unconst b)))
                     (/dc ((a const?) (b any?)) ((unconst b) a))
                     (/dm ((a any?) (b mem?)) (a b))
                     (/dm ((a mem?) (b any?)) (b a))
                     (/dd ((a any?) (b any?)) (a b)))
(boxdag-rule-variant x86/2op-ncom
                     (/dc ((a any?) (b const?)) (a b))
                     (/dm ((a any?) (b mem?)) (a b))
                     (/dd ((a any?) (b any?)) (a b)))
(define bd-rules
  (append
   (list
    (boxdag-rule-simple (arg-ref (argnum any?)) (get-memory (+ (get-base-ptr) (+ (const 8) (* argnum (const 4))))))
    (boxdag-rule-simple (arg (argnum number?)) (arg-ref (const argnum)))
    (boxdag-rule-calc (+ (a const?) (b const?)) (const (+ (unconst a) (unconst b))))
    (boxdag-rule-calc (* (a const?) (b const?)) (const (* (unconst a) (unconst b))))
    (boxdag-rule-advanced (a any?) (b const?) (get-memory (+ a b)) (x86/get-memory/dc a (unconst b)))
    (boxdag-rule-advanced (a const?) (b any?) (get-memory (+ a b)) (x86/get-memory/dc b (unconst a)))
    (boxdag-rule-advanced (a any?) (b any?) (get-memory (+ a b)) (x86/get-memory/dd a b))
    (boxdag-rule-calc (unconst (x const?)) (unconst x)))
   ; WORKING ON MEMORY REFERENCES...
   ; instr/dd, instr/dc, instr/cm, instr/cc, instr/mc, etc. d: dynamic, c: constant, m: memory
   (x86/2op-comu + x86/add)
   (x86/2op-ncom - x86/sub)
   (x86/2op-comu and x86/and)
   (x86/2op-comu or x86/or)
   (list
    (boxdag-rule-simple (get-base-ptr) (reg ebp)))
   ))
;bd
(strip-boxes (get-boxdag-content bd))
(apply-boxdag-rules-all bd-rules bd)
;bd
(strip-boxes (get-boxdag-content bd))

(define-syntax-rule (instruction (name (arg type) ...)
                                 output-info
                                 behavior)
  (list 'name (cons 'arg type) ...
        'output-info
        (boxdag-rule-advanced (arg type) ...
                     stripped-behavior
                     (name arg ...))))
(define temp (instruction (x86/add/dc (dest any?) (source const?))
                           ("  add " dest ", " source)
                           (set-reg dest (+ (get-reg dest) (unconst source)))))
;                           (yields
;                            (rule
;                             (source any?) (dest any?)
;                             (+ dest source)
;                             (x86/add/dc dest source)))
