#lang racket

(require "itemizer.rkt")
(require "x86.rkt")

; Let's start with a simple tree that we'll get from the parser/front-end generator.
(define sample '(fib (u4) u4
                     ((branch (<= (arg 0) (const 1 u4)) 1 2))
                     ((return (const 1 u4)))
                     ((return (+ (call fib (- (arg 0) (const 1 u4)))
                                 (call fib (- (arg 0) (const 2 u4))))))))

; Not currently used.
(define reduced-old '(fib (u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (<= (swr 0) (const 1 u4)))
                       (branch (swr 1) 1 2))
                      ((swr 0 (const 1 u4))
                       (return (swr 0)))
                      ((swr 0 (arg 0))
                       (swr 2 (- (swr 0) (const 1 u4)))
                       (swr 3 (call fib (swr 2)))
                       (swr 5 (- (swr 0) (const 2 u4)))
                       (swr 6 (call fib (swr 5)))
                       (swr 7 (+ (swr 3) (swr 6)))
                       (return (swr 7)))))

; Not currently used.
(define x86-simple-near '(fib (u4) u4
                              ((mov eax (mem esp 4))
                               (cmp eax (dword 1))
                               (jle 1)
                               (jmp 2))
                              ((mov eax (dword 1))
                               (ret))
                              ((mov eax (mem esp 4))
                               (sub eax (dword 1))
                               (push eax)
                               (call fib)
                               (add esp 4)
                               (mov ecx (mem esp 4))
                               (sub ecx (dword 1))
                               (push eax)
                               (push ecx)
                               (call fib)
                               (add esp 4)
                               (pop ecx)
                               (add eax ecx)
                               (ret))))

sample
(define reduced (itemize sample))
reduced
(define allocd (x86-register-allocation reduced))
allocd
(define medium (cdr (x86-transform allocd)))
medium
(define flat (x86-asm-flatten medium))
flat
(define mitigated (x86-asm-add-mitigation (car allocd) flat))
mitigated
(display (x86-asm-write-flat mitigated))
