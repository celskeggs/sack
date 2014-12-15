#lang racket

(require "boxdag.rkt")
(require "platform.rkt")
(require "platform-templates.rkt")

(platform x86
          (register-based eax ebx ecx edx esi edi)
          (argument-behavior argid (get-memory (+ (get-reg ebp) (+ (const 8 u4) (* (const argid u4) (const 4 u4))))))
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
          (reduce-<=) (reduce->=) (reduce-branch-invert)
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
           
           [(x86/sub/dc (dest any?) (source const?))
            ("  sub " dest ", " source)
            (set-reg dest (- (get-reg dest) source))]
           
           [(x86/sub/dd (dest any?) (source any?))
            ("  sub " dest ", " source)
            (set-reg dest (- (get-reg dest) (get-reg source)))]
           
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
           
           [(x86/cmp/dd (a any?) (b any?)) ; NAMEP, minus /es
            ("  cmp " a ", " b)
            (multiple
             (set-reg carry-flag (unsigned< (get-reg a) (get-reg b)))
             (set-reg zero-flag (= (get-reg a) (get-reg b)))
             (set-reg sign-flag-xor-overflow-flag (< (get-reg a) (get-reg b)))
             )]
           
           ; Remember when adding register allocation:
           ; the name forces the register to be eax.
           [(x86/ret (eax any?))
            ("  ret")
            (return (get-reg eax))]
           
           [(x86/mov/d (dest any?) (source any?))
            ("  mov " dest ", " source)
            (set-reg dest (get-reg source))] ; should yield a nullary rule.
           
           [(x86/mov/c (dest any?) (source const?))
            ("  mov " dest ", " source)
            (set-reg dest source)]
           
           [(x86/jmp (target number?))
            ("  jmp " target)
            (goto target)]
           
           [(x86/j*z (source number?))
            ("  j*z " source)
            (goto-if-not source)]
           ))

(define sample '(fib ((n u4)) u4
                     (if (<= n 1)
                         1
                         (+ (fib (- n 1))
                            (fib (- n 2))))))
(define math-sample '(math ((a u4) (b u4)) u4
                           (+ a b)))
(displayln "=== INPUT ===")
sample
(displayln "=== OUTPUT ===")
(platform-parse x86 sample)
