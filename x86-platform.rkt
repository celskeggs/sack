#lang racket

(define sample-code '(math (u4 u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (arg 1))
                       (swr 2 (+ (swr 0) (swr 1)))
                       (return (swr 2)))))

(define sample-instruction '((x86/add/dc (register dest) (register source))
                             ("  add " dest ", " source)
                             (behavior
                              (set-reg dest (+ (get-reg dest) (get-reg source))))))
