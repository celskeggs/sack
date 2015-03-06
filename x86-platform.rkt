#lang racket
(require "utilities.rkt")
(require "platform.rkt")
(require "platform-templates.rkt")
(provide x86)
(platform x86
          (register-based x86/mov/d (eax ecx edx ebx esi edi))
          (argument-behavior argid
                             (get-memory  (+ (get-reg ebp) (+ (const 8 u4) (* (const argid u4) (const 4 u4)))))
                             value
                             (set-memory! (+ (get-reg ebp) (+ (const 8 u4) (* (const argid u4) (const 4 u4)))) value))
          (localvar-behavior (varid argcount)
                             (get-memory  (- (get-reg ebp) (* (const varid u4) (const 4 u4))))
                             value
                             (set-memory! (- (get-reg ebp) (* (const varid u4) (const 4 u4))) value))
          (label-framing-code (blockid get-export)
                              (".c" blockid ":")
                              ())
          (function-framing-code (name touched get-export)
                                 ("section .text\n"
                                  "global _" name "\n"
                                  (string-append*
                                   (for/list ((import-name (map car (get-export 'imports)))
                                              #:unless (eq? import-name name))
                                     (string-append "extern _" (symbol->string import-name) "\n")))
                                  "_" name ":\n"
                                  "  push ebp\n"
                                  "  mov ebp, esp\n"
                                  "  sub esp, 4*" (get-num-used-locals get-export) "\n"
                                  (string-append*
                                   (map-curry format "  push ~s\n"
                                              (filter-not (lambda (x)
                                                            (member x '(eax ecx edx)))
                                                          touched))))
                                 (".ret:\n"
                                  (string-append*
                                   (map-curry format "  pop ~s\n"
                                              (filter-not (lambda (x)
                                                            (member x '(eax ecx edx)))
                                                          (reverse touched))))
                                  "  mov esp, ebp\n"
                                  "  pop ebp\n"
                                  "  ret\n"
                                  "section .rodata\n"
                                  (string-append*
                                   (for/list ((ptr-and-string (get-export 'strings)))
                                     (string-append
                                      "global " (symbol->string (car ptr-and-string)) "\n"
                                      (symbol->string (car ptr-and-string)) ": db "
                                      (string-join (map ~a (suffix (bytes->list (string->bytes/utf-8 (second ptr-and-string))) 0)) ", ")
                                      "\n")))))
          (call-behavior-backward     ; order of arguments
           (arg (callstack/push arg)) ; handle adding arguments
           (arg (callstack/pop)))     ; handle removing arguments
          (use-standard-reductions)
          (reduce-<=) (reduce->=) (reduce-branch-invert) (reduce->-flip)
          (strings-as-pointers) (pointers-as-numbers u4)
          (booleans-as-0-1)
          ; nasm requires stating symbols to import.
          (reduction-simple (call-raw (target symbol?))
                            (call-raw-imported (boxdag/export imports target target)))
          ; divide and modulus require a bit of special handling.
          (reduction-simple (/ (a any?) (b any?))
                            (tall-div (const 0 u4) a b))
          (reduction-simple (% (a any?) (b any?))
                            (tall-rem (const 0 u4) a b))
          (instructions
           [(x86/movfm/c (dest any?) (source const?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (get-memory source))]
           [(x86/movfm/d+c (dest any?) (source any?) (offset const?))
            ("  mov " dest ", [" source "+" offset "]")
            (set-reg dest (get-memory (+ (get-reg source) offset)))]
           [(x86/movfm/d-c (dest any?) (source any?) (offset const?))
            ("  mov " dest ", [" source "-" offset "]")
            (set-reg dest (get-memory (- (get-reg source) offset)))]
           [(x86/movfm/d (dest any?) (source any?))
            ("  mov " dest ", [" source "]")
            (set-reg dest (get-memory (get-reg source)))]
           [(x86/movtm/dc (dest any?) (source const?))
            ("  mov [" dest "], dword " source)
            (set-memory! (get-reg dest) source)]
           [(x86/movtm/dd (dest any?) (source any?))
            ("  mov [" dest "], " source)
            (set-memory! (get-reg dest) (get-reg source))]
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
           [(x86/mul (by any?))
            ("  mul " by)
            (multiple
             (set-reg eax (* (get-reg eax) (get-reg by)))
             ; not truly undefined, but we don't need the high-order bits.
             (set-reg edx (generic/undefined)))]
           [(x86/div (by any?))
            ("  div " by)
            (multiple
             (set-reg eax (tall-div (get-reg edx) (get-reg eax) (get-reg by)))
             (set-reg edx (tall-rem (get-reg edx) (get-reg eax) (get-reg by))))]
           [(x86/call (target symbol?))
            ("  call _" target)
            (multiple
             (set-reg eax (call-raw-imported target))
             (set-reg ecx (generic/undefined))
             (set-reg edx (generic/undefined))
             )]
           [(x86/push/c (source const?))
            ("  push " source)
            (discard (callstack/push source))]
           [(x86/push/d (source any?))
            ("  push " source)
            (discard (callstack/push (get-reg source)))]
           [(x86/pop (dest any?))
            ("  pop " dest)
            (set-reg dest (callstack/pop))]
           [(x86/cmp/dc (a any?) (b const?))
            ("  cmp " a ", " b)
            (multiple
             (set-reg carry-flag (unsigned< (get-reg a) b))
             (set-reg zero-flag (== (get-reg a) b))
             (set-reg sign-flag-xor-overflow-flag (< (get-reg a) b))
             )]
           [(x86/cmp/dd (a any?) (b any?)) ; NAMEP, minus /es
            ("  cmp " a ", " b)
            (multiple
             (set-reg carry-flag (unsigned< (get-reg a) (get-reg b)))
             (set-reg zero-flag (== (get-reg a) (get-reg b)))
             (set-reg sign-flag-xor-overflow-flag (< (get-reg a) (get-reg b)))
             )]
           [(x86/mov/d (dest any?) (source any?))
            ("  mov " dest ", " source)
            (set-reg dest (get-reg source))]
           [(x86/mov/c (dest any?) (source const?))
            ("  mov " dest ", " source)
            (set-reg dest source)]
           [(x86/jmp (target number?))
            ("  jmp .c" target)
            (goto target)]
           [(x86/je (source any?) (target number?))
            ("  je .c" target)
            (goto-if (generic/subresult '_ 'zero-flag (get-reg source)) target)]
           [(x86/jne (source any?) (target number?))
            ("  jne .c" target)
            (goto-if-not (generic/subresult '_ 'zero-flag (get-reg source)) target)]
           [(x86/jb (source any?) (target number?))
            ("  jb .c" target)
            (goto-if (generic/subresult '_ 'carry-flag (get-reg source)) target)]
           [(x86/jae (source any?) (target number?))
            ("  jnb .c" target)
            (goto-if-not (generic/subresult '_ 'carry-flag (get-reg source)) target)]
           [(x86/jbe (source any?) (target number?))
            ("  jbe .c" target)
            (goto-if (logical/or (generic/subresult '_ 'zero-flag (get-reg source)) (generic/subresult '_ 'carry-flag (get-reg source))) target)]
           [(x86/ja (source any?) (target number?))
            ("  ja .c" target)
            (goto-if-not (logical/or (generic/subresult '_ 'zero-flag (get-reg source)) (generic/subresult '_ 'carry-flag (get-reg source))) target)]
           [(x86/jl (source any?) (target number?))
            ("  jl .c" target)
            (goto-if (generic/subresult '_ 'sign-flag-xor-overflow-flag (get-reg source)) target)]
           [(x86/jge (source any?) (target number?))
            ("  jge .c" target)
            (goto-if-not (generic/subresult '_ 'sign-flag-xor-overflow-flag (get-reg source)) target)]
           [(x86/jle (source any?) (target number?))
            ("  jle .c" target)
            (goto-if (logical/or (generic/subresult '_ 'zero-flag (get-reg source)) (generic/subresult '_ 'sign-flag-xor-overflow-flag (get-reg source))) target)]
           [(x86/jg (source any?) (target number?))
            ("  jg .c" target)
            (goto-if-not (logical/or (generic/subresult '_ 'zero-flag (get-reg source)) (generic/subresult '_ 'sign-flag-xor-overflow-flag (get-reg source))) target)]
           [(x86/ret)
            ("  jmp .ret")
            (return (get-reg eax))]
           [(x86/jecxz (target number?))
            ("  jecxz .c" target)
            (goto-if-not (get-reg ecx) target)]
           ))
