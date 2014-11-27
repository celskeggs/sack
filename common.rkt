#lang racket

(provide any? const? wrap-const)

(define (any? x)
  #t)
(define (const? x)
  (integer? x))
(define (wrap-const x)
  (list 'const x 'u4)) ; TODO: don't hard-code types
