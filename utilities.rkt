#lang racket

(provide zip unzip without assert tree->string trace)

(define (zip a b)
  (cond ((and (empty? a) (empty? b)) empty)
        ((or (empty? a) (empty? b)) (error "Length mismatch to zip!"))
        (else (cons (list (car a) (car b)) (zip (cdr a) (cdr b))))))

(define (unzip x)
  (apply map (cons list x)))

(define (tree->string inst)
  (define o (open-output-string))
  (display inst o)
  (get-output-string o))

(define (without from remove)
  (filter (lambda (x) (not (member x remove))) from))

(define (trace . args)
  (print args)
  (newline))

(define (assert x)
  (if x
      x
      (error "Assertion failed")))