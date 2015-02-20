#lang racket

(require "pipeline.rkt")
(require "platform.rkt")
(require "x86-platform.rkt")

(define math-test '(math ((a u4) (b u4)) u4
                         (+ a b)))

(define fib '(fib ((n u4)) u4
                  (if (< n 2)
                      1
                      (+ (fib (- n 1))
                         (fib (- n 2))))))

(define tracing-fib '(fib ((n u4)) u4
                          (printf "Fib @ %d\n" n)
                          (if (< n 2)
                              1
                              (+ (fib (- n 2))
                                 (fib (- n 1))))))

(define hello-world '(hello () v
                            (puts "HELLO WORLD!")))

(define tracing-main '(main () u4
                            (def i u4)
                            (set! i 0)
                            (while (< i 10)
                                   (printf "fib(%d) = %d\n" i (fib i))
                                   (set! i (+ 1 i)))
                            0))

(run-platform-pipeline x86 fib)

;(require "platform-templates.rkt")
;(platform tiny-test (use-standard-reductions))
;(run-platform-pipeline tiny-test '(test ((i u4)) u4 (- i 0)))
