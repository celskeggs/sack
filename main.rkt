#lang racket

(require "pipeline.rkt")
(require "platform.rkt")
(require "x86-platform.rkt")

(define math-test '(math ((a u4) (b u4)) u4
                         (+ a b)))

(define fib '(fib ((n u4)) u4
                  ;(printf "Fib @ %d" n)
                  (if (< n 2)
                      1
                      (+ (fib (- n 1))
                         (fib (- n 2))))))

(define tracing-fib '(fib ((n u4)) u4
                          (printf "Fib @ %d" n)
                          (if (< n 2)
                              1
                              (+ (fib (- n 1))
                                 (fib (- n 2))))))

(run-platform-pipeline x86 tracing-fib)
