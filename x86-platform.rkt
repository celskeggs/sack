#lang racket

(require "boxdag.rkt")
(require "platform.rkt")
(require "platform-templates.rkt")
;(require "register-allocation.rkt")

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
            ("  jmp " target)
            (goto target)]
           
           [(x86/je (source any?) (target number?))
            ("  je " target)
            (goto-if (generic/subresult _ zero-flag source) target)]
           
           [(x86/jne (source any?) (target number?))
            ("  jne " target)
            (goto-if-not (generic/subresult _ zero-flag source) target)]
           
           [(x86/jb (source any?) (target number?))
            ("  jb " target)
            (goto-if (generic/subresult _ carry-flag source) target)]
           
           [(x86/jae (source any?) (target number?))
            ("  jnb " target)
            (goto-if-not (generic/subresult _ carry-flag source) target)]
           
           [(x86/jbe (source any?) (target number?))
            ("  jbe " target)
            (goto-if (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ carry-flag source)) target)]
           
           [(x86/ja (source any?) (target number?))
            ("  ja " target)
            (goto-if-not (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ carry-flag source)) target)]
           
           [(x86/jl (source any?) (target number?))
            ("  jl " target)
            (goto-if (generic/subresult _ sign-flag-xor-overflow-flag source) target)]
           
           [(x86/jge (source any?) (target number?))
            ("  jge " target)
            (goto-if-not (generic/subresult _ sign-flag-xor-overflow-flag source) target)]
           
           [(x86/jle (source any?) (target number?))
            ("  jle " target)
            (goto-if (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ sign-flag-xor-overflow-flag source)) target)]
           
           [(x86/jg (source any?) (target number?))
            ("  jg " target)
            (goto-if-not (logical/or (generic/subresult _ zero-flag source) (generic/subresult _ sign-flag-xor-overflow-flag source)) target)]
           
           [(x86/jecxz (source any?) (target number?))
            ("  jecxz " source target)
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
conv
(define (get-instruction name)
  (car (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs x86))))
(define (single-constraints out-ssa name arguments)
  (let ((instr (get-instruction name)))
    (list out-ssa arguments (instruction-struct-arguments instr) (instruction-struct-constraints instr))))
(single-constraints 4 'x86/call '(fib))
(define (ssa-constraints blk)
  (single-constraints (caar blk) (cadar blk) (caddar blk))
  )
(ssa-constraints (cdr (cadddr conv)))
;(register-allocate '(eax ebx ecx edx esi edi) conv)
