#lang racket

(require "utilities.rkt")
(require "boxdag.rkt")
(require "platform.rkt")
(require "platform-templates.rkt")
(require "register-constraints.rkt")
(require "register-allocation.rkt")

(platform x86
          (register-based x86/mov/d (eax ebx ecx edx esi edi))
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
           [(x86/ret)
            ("  ret")
            (return (get-reg eax))]
           
           [(x86/mov/d (dest any?) (source any?))
            ("  mov " dest ", " source)
            (set-reg dest (get-reg source))] ; should yield a nullary rule.
           
           [(x86/mov/c (dest any?) (source const?))
            ("  mov " dest ", " source)
            (set-reg dest source)]
           
           [(x86/jmp (target number?))
            ("  jmp .c" target)
            (goto target)]
           
           [(x86/je (source any?) (target number?))
            ("  je .c" target)
            (goto-if (generic/subresult _ zero-flag source) target)]
           
           [(x86/jne (source any?) (target number?))
            ("  jne .c" target)
            (goto-if-not (generic/subresult _ zero-flag source) target)]
           
           [(x86/jb (source any?) (target number?))
            ("  jb .c" target)
            (goto-if (generic/subresult _ carry-flag source) target)]
           
           [(x86/jae (source any?) (target number?))
            ("  jnb .c" target)
            (goto-if-not (generic/subresult _ carry-flag source) target)]
           
           [(x86/jbe (source any?) (target number?))
            ("  jbe .c" target)
            (goto-if (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ carry-flag source)) target)]
           
           [(x86/ja (source any?) (target number?))
            ("  ja .c" target)
            (goto-if-not (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ carry-flag source)) target)]
           
           [(x86/jl (source any?) (target number?))
            ("  jl .c" target)
            (goto-if (generic/subresult _ sign-flag-xor-overflow-flag source) target)]
           
           [(x86/jge (source any?) (target number?))
            ("  jge .c" target)
            (goto-if-not (generic/subresult _ sign-flag-xor-overflow-flag source) target)]
           
           [(x86/jle (source any?) (target number?))
            ("  jle .c" target)
            (goto-if (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ sign-flag-xor-overflow-flag source)) target)]
           
           [(x86/jg (source any?) (target number?))
            ("  jg .c" target)
            (goto-if-not (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ sign-flag-xor-overflow-flag source)) target)]
           
           [(x86/jecxz (source any?) (target number?))
            ("  jecxz .c" source target)
            (goto-if-not source target)]
           ))

(define sample '(fib ((n u4)) u4
                     (if (< n 2)
                         1
                         (+ (fib (- n 1))
                            (fib (- n 2))))))
(define math-sample '(math ((a u4) (b u4)) u4
                           (+ a b)))
(displayln "=== INPUT ===")
sample
(displayln "=== OUTPUT ===")
(define conv (platform-parse x86 sample))
;conv
;(register-constrain x86 conv)
(register-allocate x86 '(eax ebx ecx edx esi edi) conv)
