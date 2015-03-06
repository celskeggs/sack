#lang racket

(require "pipeline.rkt")
(require "platform.rkt")
(require "x86-platform.rkt")
(require "tstk-platform.rkt")
(require "jvm-platform.rkt")
(require "utilities.rkt")

(define math-test '(math ((a u4) (b u4)) u4
                         (* a (+ 3 b))))

(define minimal '(minimal ((n u4)) u4
                          (+ (minimal (- n 1))
                             (minimal (- n 2)))))

(define fib '(fib ((n u4)) u4
                  (if (< n 2)
                      1
                      (+ (fib (- n 1))
                         (fib (- n 2))))))

(define timing-main '(main () u4
                           (printf "Fib(44) = %d\n" (fib 44))
                           0))

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
                            (while (<= i 44)
                                   (printf "fib(%d) = %d\n" i (fib i))
                                   (set! i (+ 1 i)))
                            0))
(define tracing-main-for-jvm '(main () u4
                                    (def i u4)
                                    (set! i 0)
                                    (while (< i 10)
                                           (jvm/extcall "printf(Ljava/lang/String;II)I" "fib(%d) = %d\n" i (fib i))
                                           (set! i (+ 1 i)))
                                    0))

(define var-test '(test () u4
                        (def i u4)
                        (set! i 0)
                        0))

(define tmp-test '(test () u4
                        (+ 0 (fib 0))))

(define ack '(ack ((m u4) (n u4)) u4
                  (if (== m 0)
                      (+ n 1)
                      (if (logical/and (> m 0) (== n 0))
                          (ack (- m 1) 1)
                          (ack (- m 1) (ack m (- n 1)))))))
(define ack-main '(main () u4
                        (def i u4)
                        (def j u4)
                        (set! i 0)
                        (while (<= i 4)
                               (set! j 0)
                               (while (<= j 4)
                                      (printf "ack(%d, %d) = %d\n" i j (ack i j))
                                      (set! j (+ j 1)))
                               (set! i (+ i 1)))
                        0))

; prime finder!
(define prime-finder '(main () u4
                            (printf "2")
                            (def i u4)
                            (def count u4)
                            (set! i 3)
                            (set! count 1)
                            (while (<= i 10000000)
                                   (def divisor u4)
                                   (set! divisor 3)
                                   (def isprime b)
                                   (set! isprime #t)
                                   (while (logical/and isprime (<= (* divisor divisor) i))
                                          (when (logical/not (% i divisor))
                                            (set! isprime #f))
                                          (set! divisor (+ divisor 2)))
                                   (when isprime
                                     (when (logical/not (% count 10000))
                                       (printf " %d" i))
                                     (set! count (+ count 1)))
                                   (set! i (+ i 2)))
                            (printf "\n")
                            0))

;(run-platform-pipeline x86 tracing-main)
;(run-platform-pipeline tstk tracing-main)
(comment (define (jvm-compile code)
  (run-platform-pipeline jvm code #:provide-result 'textual-assembly))
(let ((gotten (map jvm-compile (list tracing-main-for-jvm fib))))
  (displayln "=== LINKED ===")
  (display (jvm-link "com/colbyskeggs/sack/Test"
                     gotten
                     #:include-main #t))))
(require "platform-structures.rkt")
