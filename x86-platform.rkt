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

;(define bd1 (make-boxdag '(+ (const 8) (* (const 0) (const 4)))))
(define bd1 (make-boxdag '(+ (arg 0) (const 10))))
(strip-boxes (get-boxdag-contents (platform-apply x86 bd1)))
(define bd2 (platform-process x86 '(call hello (call a (arg 1)) (call b (const 30)))))
;(define bd2 (platform-process x86 '(call hello (arg 1) (const 30))))
;(define bd2 (platform-process x86 '(call hello (const 30))))
(strip-boxes (get-boxdag-contents bd2))
