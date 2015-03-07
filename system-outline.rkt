#lang racket

(require "utilities.rkt")

(struct fragment-handler (target-predicate combiner) #:inspector #f)
(struct system-type (name handlers))

(define (construct-system fragment-handlers fragments)
  (if (empty? fragment-handlers)
      (if (empty? fragments)
          empty
          (error "Unhandlable fragment: " (car fragments)))
      (let-values (((current remaining) (partition (fragment-handler-target-predicate (car fragment-handlers)) fragments)))
        (cons ((fragment-handler-combiner (car fragment-handlers)) current)
              (construct-system (cdr fragment-handlers) remaining)))))

(define-syntax (define-system-type stx)
  (syntax-case stx ()
    [(define-system-type typename (hndl-name handler) ...)
     (let ((ps-name (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum #'typename)) "-struct")))))
       #`(begin
           (define fragment-handlers (map (lambda (x) (if (fragment-handler? x) x (error "Not a fragment handler: " x)))
                                          (list handler ...)))
           (struct #,ps-name (hndl-name ...) #:inspector #f)
           (define-syntax-rule (typename name code (... ...))
             (define name (apply #,ps-name (construct-system fragment-handlers
                                                             (filter-not void? (list code (... ...)))))))))]))

(define-system-type my-system-type
  (sym (fragment-handler symbol? list))
  (int (fragment-handler number? (lambda (x) (apply + x)))))

(my-system-type test
                10
                'foo
                0
                'bar
                6
                'baz
                11000)
