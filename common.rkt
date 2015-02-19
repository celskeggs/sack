#lang racket

(require "utilities.rkt")

(provide any? const? wrap-const const-ref const-ref-refname const-ref?)

(struct const-ref (refname) #:inspector #f)

(define (any? x)
  #t)
(define (const? x)
  (or (integer? x) (const-ref? x)))
(define (wrap-const x)
  (list 'const x 'u4)) ; TODO: don't hard-code types
