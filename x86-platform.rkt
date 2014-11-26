#lang racket

(require "boxdag.rkt")
(require "platform.rkt")

(platform x86
          (register-based eax ebx ecx edx esi edi)
          (argument-behavior argid (get-memory (+ (get-reg ebp) (+ (const 8) (* (const argid) (const 4))))))
          (reduction-simple (get-memory (c const?))
                            (x86/get-memory/c c))
          (reduction-advanced (a any?) (b const?)
                              (get-memory (+ a b))
                              (x86/get-memory/dc a b))
          (reduction-simple (get-memory (a any?))
                            (x86/get-memory/d a))
          (call-behavior-forward ; first argument to last argument
           (arg (push arg)) ; handle adding arguments
           (arg (pop))) ; handle removing arguments
          (use-standard-reductions)
          (instructions
           [(x86/movfm/c (dest any?) (source const?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (x86/get-memory/c source))]
           
           [(x86/movfm/d (dest any?) (source any?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (x86/get-memory/d (get-reg source)))]
           
           [(x86/add/dc (dest any?) (source const?))
            ("  add " dest ", " source)
            (set-reg dest (+ (get-reg dest) source))]
           
           [(x86/add/dd (dest any?) (source any?))
            ("  add " dest ", " source)
            (set-reg dest (+ (get-reg dest) (get-reg source)))]
           
           [(x86/call (target symbol?))
            ("  call " target)
            (set-reg eax (call-raw target))]
           
           [(x86/push/c (source const?))
            ("  push " source)
            (discard (push source))]
           
           [(x86/push/d (source any?))
            ("  push " source)
            (discard (push (get-reg source)))]
           
           [(x86/pop (dest any?))
            ("  pop " dest)
            (set-reg dest (pop))]
           
           [(x86/mov/d (dest any?) (source any?))
            ("  mov " dest ", " source)
            (set-reg dest (get-reg source))] ; should yield a nullary rule.
           
           [(x86/mov/c (dest any?) (source const?))
            ("  mov " dest ", " source)
            (set-reg dest source)]
           ))

(define sample '(fib ((n u4)) u4
                     (if (<= n 1)
                         1
                         (+ (fib (- n 1))
                            (fib (- n 2))))))
(define math-sample '(math ((a u4) (b u4)) u4
                           (+ a b)))
(platform-parse x86 sample)
