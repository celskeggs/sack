#lang racket

(define sample '(math (u4 u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (arg 1))
                       (swr 2 (+ (swr 0) (swr 1)))
                       (return (swr 2)))))

; x86 information
(platform x86
          (registers eax ebx ecx edx esi edi)
          (return-behavior returned-value
                           (set-register eax returned-value))
          (argument-behavior argnum
                             (get-memory (+ (get-register ebp) (+ 8 (* argnum 4)))))
          (instruction-delimiter "\n")
          (definition-syntax "global " name "\n" name ":")
          (jumptarget-syntax ".lab" name ":")
          (instructions
           (inst (mov (register dest) (memory source))
                 ("  mov " dest ", [" source "]")
                 (behavior
                  (set-register dest (get-memory source))))
           (inst (add (register dest) (register source))
                 ("  add " dest ", " source)
                 (behavior
                  (set-register dest (+ (get-register source) (get-register dest)))))
           (inst (mov (register dest) (register source))
                 ("  mov " dest ", " source)
                 (behavior
                  (set-register dest (get-register source))))
           ))

(reduction-behavior
 (replace (* (number a) (number b))
          (lambda (a b) (* a b)))
 (replace (+ (number a) (number b))
          (lambda (a b) (+ a b))))

(define reduced '((return (+ (arg 0) (arg 1)))
                  (set-register eax (+ (arg 0) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) (+ 8 (* 0 4)))) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) (+ 8 0))) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) (+ 8 (* 1 4))))))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) (+ 8 4)))))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) 12))))
                  ))
