#lang racket

(provide platform register-based argument-behavior use-standard-reductions reduction-simple reduction-advanced reduction-calc
         const? any? instructions platform-apply platform-process call-behavior-forward platform-parse)

(require racket/stxparam)
(require "utilities.rkt")
(require "common.rkt")
(require "boxdag.rkt")
(require "boxdag-rules.rkt")
(require "parser.rkt")
(require "boxdag-to-swr.rkt")
(require "rule-generator.rkt")

(struct mutable-platform-struct
  (name registers instrs rules) #:mutable #:inspector #f)
(struct platform-struct
  (name registers instrs rules) #:inspector #f)
(define (finalize-platform x) ; TODO
  (platform-struct (mutable-platform-struct-name x)
                   (mutable-platform-struct-registers x)
                   (reverse (mutable-platform-struct-instrs x))
                   (reverse (mutable-platform-struct-rules x))))

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
(define (is-trivial-rule rule)
  (and (symbol? (boxdag-rule-find rule))
       (= (length (boxdag-rule-args rule)) 1)
       (equal? (cdar (boxdag-rule-args rule)) any?)))
(define-syntax-rule (instructions instr ...)
  (begin
    (begin
      (let ((i-struct (instruction . instr)))
        (add-platform-instr! active-platform-ref i-struct)
        (for ([rule (instruction-struct-rules i-struct)]
              #:unless (is-trivial-rule rule))
          (add-platform-rule! active-platform-ref rule)))) ...))
(define-syntax-rule (use-standard-reductions)
  (begin
    (reduction-calc (+ (a const?) (b const?)) (wrap-const (+ a b)))
    (reduction-calc (* (a const?) (b const?)) (wrap-const (* a b)))
    (reduction-advanced (x any?) (generic/middle-of (generic/middle x)) x)
    (reduction-advanced (x any?) (rest pair?) (generic/middle-of (generic/middle x) . rest) x)
    (reduction-advanced (x any?) (rest pair?) (generic/middle-of x . rest) (generic/middle-of . rest))
    ))
(define-syntax-rule (call-behavior-forward (argn pushexpr) (argn2 popexpr))
  (begin
    (reduction-simple (generic/call-argument-add (argn any?))
                      pushexpr)
    (reduction-simple (generic/call-argument-remove (argn2 any?))
                      popexpr)
    (add-platform-rule! active-platform-ref
                        (boxdag-rule
                         (list (cons 'target symbol?) (cons 'args list?))
                         '(call target . args)
                         (lambda (vars)
                           (let ((target (cdr (assoc 'target vars)))
                                 (args (cdr (assoc 'args vars))))
                             (append '(generic/middle-of)
                                     (map (lambda (arg) (list 'boxdag/preserve (list 'generic/call-argument-add arg))) args)
                                     `((generic/middle (boxdag/preserve (call-raw ,target))))
                                     (map (lambda (arg) (list 'boxdag/preserve (list 'generic/call-argument-remove arg))) args))))))
    ))
(define-syntax-rule (platform name entry ...)
  (define name (let ((platform-def (mutable-platform-struct 'name (void) empty empty))
                     (is-register? (lambda (x) (error "No (register-based) declaration!"))))
                 (syntax-parameterize ([active-platform-ref (make-rename-transformer #'platform-def)]
                                       [register? (make-rename-transformer #'is-register?)])
                                      entry ...)
                 (finalize-platform platform-def))))

(define (platform-parse platform input)
  (let* ((parsed (parse input))
         (name (first parsed))
         (args (second parsed))
         (rettype (third parsed))
         (body (cdddr parsed)))
    (cons name (cons args (cons rettype (map (curry platform-process-block platform) body))))))
(define (platform-process-block platform block)
  (assert (= (length block) 1) "Can only process one statement per block. (TODO)")
  (full-swrify (get-boxdag-contents (platform-process platform (car block)))))
(define (platform-apply platform boxdag)
  (apply-boxdag-rules-all (platform-struct-rules platform) boxdag)
  boxdag)
(define (platform-process platform input)
  (platform-apply platform (make-boxdag input)))
(define (calc-fixup-arg arg cond)
  (if (eq? cond const?)
      (wrap-const arg) ; TODO: don't hard-code types
      arg))
(define (calc-fixup-recurse args data)
  (cond ((pair? data)
         (cons (calc-fixup-recurse args (car data))
               (calc-fixup-recurse args (cdr data))))
        ((and (assoc data args) (eq? (cdr (assoc data args)) const?))
         (wrap-const data))
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
