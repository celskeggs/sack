#lang racket

(provide register-based argument-behavior use-standard-reductions call-behavior-forward)

(require "common.rkt")
(require "platform.rkt")

(define-syntax-rule (register-based reg ...)
  (begin
    (define registers '(reg ...))
    (set! register? (lambda (r) (and (member r registers) #t)))
    (set-registers! registers)))
(define-syntax-rule (argument-behavior argname contents)
  (reduction-simple (arg (argname any?)) contents))
(define-syntax-rule (use-standard-reductions)
  (begin
    (reduction-calc (+ (a const?) (b const?)) (wrap-const (+ a b)))
    (reduction-calc (* (a const?) (b const?)) (wrap-const (* a b)))
    (reduction-advanced (x any?) (generic/middle-of (generic/middle x)) x)
    (reduction-advanced (x any?) (rest pair?) (generic/middle-of (generic/middle x) . rest) x)
    (reduction-advanced (x any?) (rest pair?) (generic/middle-of x . rest) (generic/middle-of . rest))))
(define-syntax-rule (call-behavior-forward (argn pushexpr) (argn2 popexpr))
  (begin
    (reduction-simple (generic/call-argument-add (argn any?))
                      pushexpr)
    (reduction-simple (generic/call-argument-remove (argn2 any?))
                      popexpr)
    (reduction-raw (list (cons 'target symbol?) (cons 'args list?))
                   '(call target . args)
                   (lambda (vars)
                     (let ((target (cdr (assoc 'target vars)))
                           (args (cdr (assoc 'args vars))))
                       (append '(generic/middle-of)
                               (map (lambda (arg) (list 'boxdag/preserve (list 'generic/call-argument-add arg))) args)
                               `((generic/middle (boxdag/preserve (call-raw ,target))))
                               (map (lambda (arg) (list 'boxdag/preserve (list 'generic/call-argument-remove arg))) args)))))))
