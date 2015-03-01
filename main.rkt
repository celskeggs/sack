#lang racket

(require "pipeline.rkt")
(require "platform.rkt")
(require "x86-platform.rkt")
(require "tstk-platform.rkt")

(define math-test '(math ((a u4) (b u4)) u4
                         (- a (+ 3 b))))

(define minimal '(minimal ((n u4)) u4
                          (+ (minimal (- n 1))
                             (minimal (- n 2)))))

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

(define var-test '(test () u4
                        (def i u4)
                        (set! i 0)
                        0))

(define tmp-test '(test () u4
                        (+ 0 (fib 0))))

;(run-platform-pipeline x86 tracing-main)
(run-platform-pipeline tstk tmp-test #:target 'ssa-assembly-with-exports) ; <<--- ACTIVE

;(require "platform-templates.rkt")
;(platform tiny-test (use-standard-reductions))
;(run-platform-pipeline tiny-test '(test ((i u4)) u4 (- i 0)))

(require "boxdag-rules.rkt")
(require "platform-structures.rkt")
(require "boxdag.rkt")
(require "utilities.rkt")

(comment (let ((bd (make-boxdag '(+
                                       (call minimal (- (slot-get (const 0 u4)) (const 1 u4)))
                                       (call minimal (- (slot-get (const 0 u4)) (const 2 u4)))))))
  (apply-boxdag-rule-once (list-ref (platform-struct-rules tstk) 14)
                          (void)
                          bd)
  (get-boxdag-contents bd)))
