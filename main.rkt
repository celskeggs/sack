#lang racket

(require racket/stream)
(require "utilities.rkt")

; SACK! The Semantic Automated Compiler Kit.

; Let's start with a simple tree that we'll get from the parser/front-end generator.

(define sample '(fib (u4) u4
                     ((branch (<= (arg 0) (const 1 u4)) ; 0
                             1 2))
                     ((return (const 1 u4)))
                     ((return (+ (call fib (- (arg 0) (const 1 u4)))
                                 (call fib (- (arg 0) (const 2 u4))))))))

(define reduced '(fib (u4) u4
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

; related to the next function
(define (register-allocation-insert-defers block begin-unused)
  (cond ((empty? block) empty)
        ((and (eq? (caar block) 'swr) (eq? (caaddr (car block)) 'call))
         (cons (cons 'swr (cons begin-unused (cddar block)))
               (cons (list 'swr (cadar block) (list 'swr begin-unused))
                     (register-allocation-insert-defers (cdr block) (+ 1 begin-unused)))))
        (else (cons (car block)
                    (register-allocation-insert-defers (cdr block) begin-unused)))))

; related to the previous function
(define (register-allocation-required regs block)
  (let ((return-register (car regs)))
    (filter-map (lambda (insn)
                  (cond ((and (eq? (car insn) 'swr) (eq? (caaddr insn) 'call))
                         (cons (cadr insn) return-register))
                        ((and (eq? (car insn) 'return) (eq? (caadr insn) 'swr))
                         (cons (cadadr insn) return-register))
                        (else #f)))
                block)))

(define (swr-listing block)
  (filter-map (lambda (insn)
                (if (eq? (car insn) 'swr)
                    (cadr insn)
                    #f))
              block))

(define (swr-find-unused block)
  (define (search-listing-from listing i)
    (cond ((or (empty? listing) (> (car listing) i)) i)
          ((< (car listing) i) (search-listing-from (cdr listing) i))
          (else (search-listing-from listing (+ i 1)))))
  (search-listing-from (sort (swr-listing block) <) 0))

(define (swr-find-unused-beginning block)
  (+ 1 (apply max (swr-listing block))))

(define (find-index p seq)
  (find-index-i p seq 0))

(define (find-index-i p seq i)
  (cond ((empty? seq) (error "No match in list!"))
        ((p (car seq)) i)
        (else (find-index-i p (cdr seq) (+ i 1)))))

(define (tree-search haystack needle)
  (cond ((equal? haystack needle) #t)
        ((pair? haystack) (or (tree-search (car haystack) needle)
                              (tree-search (cdr haystack) needle)))
        (else #f)))

(define (swr-usage-begin block swrid)
  (find-index (lambda (x) (and (eq? (car x) 'swr) (= (cadr x) swrid)))
              block))

(define (swr-usage-end block swrid)
  (find-index (lambda (x) (tree-search x (list 'swr swrid)))
              block))

(define (swr-usage-ranges block)
  (map (lambda (swrid)
         (list swrid (swr-usage-begin block swrid) (swr-usage-end block swrid)))
       (swr-listing block)))

(define (swr-conflicts block)
  (define (find-conflicts from to)
    (cond ((empty? to) empty)
          ((< (second (car to)) (third from)) (cons (list (first from) (first (car to)))
                                                    (find-conflicts from (cdr to))))
          (else (find-conflicts from (cdr to)))))
  (define (swr-conflicts-i active)
    (if (empty? active) empty
        (append (find-conflicts (car active) (cdr active)) (swr-conflicts-i (cdr active)))))
  (swr-conflicts-i (sort (swr-usage-ranges block) (lambda (a b) (< (second a) (second b))))))

(define (swr-preferences block)
  (define (swr-preferences-iter starts ends) ; looks through the list of starts and ends and finds places where one starts and another ends at the same time.
    (cond ((or (empty? starts) (empty? ends)) empty)
          ((< (cadar starts) (cadar ends)) (swr-preferences-iter (cdr starts) ends))
          ((> (cadar starts) (cadar ends)) (swr-preferences-iter starts (cdr ends)))
          (else ; we found a preference!
           (cons (sort (list (caar starts) (caar ends)) <)
                 (if (and (pair? (cdr ends)) (= (cadadr ends) (cadar starts)))
                     (swr-preferences-iter starts (cdr ends))
                     (swr-preferences-iter (cdr starts) ends))))))
  (let ((unzipped (unzip (map (lambda (x) (list (list (first x) (second x)) (list (first x) (third x)))) (swr-usage-ranges block)))))
    (let ((sorted-starts (sort (first unzipped) (lambda (a b) (< (second a) (second b)))))
          (sorted-ends (sort (second unzipped) (lambda (a b) (< (second a) (second b))))))
      (swr-preferences-iter sorted-starts sorted-ends))))

(define (stream-append-stream stream-stream)
  (define (s-a-s-i active more)
    (if (stream-empty? active) (stream-append-stream more)
        (stream-cons (stream-first active) (s-a-s-i (stream-rest active) more))))
  (if (stream-empty? stream-stream)
      empty-stream
      (s-a-s-i (stream-first stream-stream) (stream-rest stream-stream))))

(define (list->stream list)
  (if (empty? list) empty-stream
      (stream-cons (car list) (list->stream (cdr list)))))

(define (find-vars-in-register allocation register)
  (filter-map (lambda (kvpair)
                (if (eq? register (cdr kvpair))
                    (car kvpair)
                    #f))
              allocation))

(define (check-not-in-conflict pair val1 val2) ; if each cell contains a different one of the values
  (not (or (and (= (first pair) val1) (= (second pair) val2))
           (and (= (first pair) val2) (= (second pair) val1)))))

(define (color-solve remaining regs conflicts preferences previous-allocation)
  ; (pretty-print (list 'color-solve remaining regs conflicts preferences previous-allocation))
  (if (empty? remaining) (stream previous-allocation)
      (let* ((next (car remaining))
             (rest (cdr remaining))
             (no-conflict-i (lambda (no-conflict-i cflx conflict-register)
                              (let ((conflict-vars (find-vars-in-register previous-allocation conflict-register)))
                                (andmap (lambda (conflict-elem)
                                          (andmap (curry check-not-in-conflict conflict-elem next)
                                                  conflict-vars))
                                        cflx))))
             (without-conflicting (filter (curry no-conflict-i no-conflict-i conflicts) regs))
             (sort-by-preference (lambda (a b) (and
                                                (member a preferences)
                                                (not (member b preferences)))))
             (ordering (sort without-conflicting sort-by-preference))
             (new-allocations (map (lambda (assign) (cons (cons next assign) previous-allocation)) ordering)))
        (stream-append-stream (stream-map
                               (lambda (new-allocation)
                                   (color-solve rest regs conflicts preferences new-allocation))
                               (list->stream new-allocations))))))

(define (register-allocation-apply block lookup)
  (define (fix-tree tree)
    (if (not (pair? tree)) tree
        (if (eq? (first tree) 'swr) (list 'reg (cdr (assoc (second tree) lookup)))
            (map fix-tree tree))))
  (define (apply-line line)
    (if (eq? (first line) 'swr)
        (cons 'reg (cons (cdr (assoc (second line) lookup))
                         (fix-tree (cddr line))))
        (cons (first line) (fix-tree (cdr line)))))
  (map apply-line block))

(define (register-allocation-block regs orig-block)
  (let ((block (register-allocation-insert-defers orig-block (swr-find-unused-beginning orig-block))))
    (let ((base-allocation (register-allocation-required regs block))
          (conflicts (swr-conflicts block))
          (preferences (swr-preferences block)))
      (let ((result (stream-first (color-solve (without (swr-listing block) (map car base-allocation)) (cadr regs) conflicts preferences base-allocation))))
        ;(pretty-print (list 'result result))
        (register-allocation-apply block result)))))

(define (register-allocation regs tree)
  (cons (car tree)
        (cons (cadr tree)
              (cons (caddr tree)
                    (map (curry register-allocation-block regs) (cdddr tree))))))

(define x86-registers
  '(eax ; return register
    (eax ebx ecx edx ebp esi edi))) ; all general-purpose registers

reduced
;(map swr-conflicts (cdddr reduced))
;(map swr-usage-ranges (cdddr reduced))
;(map swr-preferences (cdddr reduced))
(register-allocation x86-registers reduced)

(define (update-vars assocl key value)
  (cond ((eq? (caar assocl) key) (cons (list key value) (cdr assocl)))
        (else (cons (car assocl) (update-vars (cdr assocl) key value)))))

(define (tree-match-i tree rule varpop)
  ;(trace 'TREE-MATCH tree rule varpop)
  (let ((var (assoc rule varpop)))
    (if var
        (if (or (null? (second var)) (eqv? (second var) tree))
            (update-vars varpop rule tree)
            #f)
        (cond ((and (pair? rule) (pair? tree))
               (let ((subret (tree-match-i (car tree) (car rule) varpop)))
                 (if subret
                     (tree-match-i (cdr tree) (cdr rule) subret)
                     subret)))
              ((eqv? rule tree)
               varpop)
              (else #f)))))
  
(define (tree-match tree vars rule)
  (if (not (pair? tree))
      #f
      (let ((out (tree-match-i tree rule (map (lambda (x) (list x null)) vars))))
        (if out
            (map (lambda (x) (assert (not (empty? x))) x) out)
            out))))

(define (fetch-all-args varlist vars)
  (map (lambda (x) (second (assoc x vars))) varlist))

(define (tree-replace tree varlist vars)
  (let ((lkup (assoc tree vars)))
    (cond (lkup (second lkup))
          ((procedure? tree) (apply tree (fetch-all-args varlist vars)))
          ((pair? tree)
           (cons (tree-replace (car tree) varlist vars)
                 (tree-replace (cdr tree) varlist vars)))
          (else tree))))

(define (tree-match-replace-single tree vars rule replace)
  (let ((match (tree-match tree vars rule)))
    (and match (tree-replace replace vars match))))

(define (tree-match-replace tree vars rule replace)
  (let ((direct (tree-match-replace-single tree vars rule replace)))
    (if direct
        direct
        (if (not (pair? tree))
            #f
            (let ((rhead (tree-match-replace (car tree) vars rule replace))
                  (rtail (tree-match-replace (cdr tree) vars rule replace)))
              (if (or rhead rtail)
                  (cons (or rhead (car tree)) (or rtail (cdr tree)))
                  #f))))))

(define (tree-transform orig-tree all-rules)
  (define (tree-transform-i tree rules)
    (if (empty? rules) tree
        (let ((replaced (tree-match-replace tree (caar rules) (cadar rules) (caddar rules))))
          (if replaced
              (tree-transform-i replaced all-rules)
              (tree-transform-i tree (cdr rules))))))
  (tree-transform-i orig-tree all-rules))

(define x86-output-prerules `(
                              (()
                               (pushall)
                               (seq))
                              ((h r)
                               (pushall h . r)
                               (seq
                                (pushone h)
                                (pushall . r)))
                              ((f a)
                               (reg eax (call f . a))
                               (seq
                                (pushall . a)
                                (call f)
                                (add esp ,(lambda (f a) (* 4 (length a))))))
                              ((r)
                               (pushone (reg r))
                               (push r))
                              ((r n)
                               (reg r (arg n))
                               (mov r (mem esp ,(lambda (r n) (* (+ n 1) 4)))))
                              ((r)
                               (reg r (reg r))
                               (seq))
                              ((ro ri)
                               (reg ro (reg ri))
                               (mov ro ri))
                              ((ro ri cst)
                               (reg ro (<= (reg ri) (const cst u4)))
                               (seq
                                (cmp ri (dword cst))
                                (setbe (reg-low-byte ro))
                                (movzbl ro (reg-low-byte ro))))
                              ((r cst)
                               (reg r (+ (reg r) (const cst u4)))
                               (add r (dword cst)))
                              ((ro ri)
                               (reg ro (+ (reg ro) (reg ri)))
                               (add ro ri))
                              ((ro ri)
                               (reg ro (+ (reg ri) (reg ro)))
                               (add ro ri))
                              ((r cst)
                               (reg r (- (reg r) (const cst u4)))
                               (sub r (dword cst)))
                              ((ro ri cst)
                               (reg ro (+ (reg ri) (const cst u4)))
                               (seq
                                (mov ro ri)
                                (add ro (dword cst))))
                              (()
                               (reg-low-byte eax)
                               al)
                              ((cond true false)
                               (branch (reg cond) true false)
                               (seq
                                (cmp cond 0)
                                (jne true)
                                (jmp false)))
                              ((r cst)
                               (reg r (const cst u4))
                               (mov r (dword cst)))
                              (()
                               (return (reg eax))
                               (ret))
                              ))

(define x86-output '(((label x) x ":")
                     ((local x) ".lab" x ":")
                     ((mov dst src) "  mov " dst ", " src)
                     ((cmp a b) "  cmp " a ", " b)
                     ((jle tgt) "  jle " ".lab" tgt)
                     ((jge tgt) "  jge " ".lab" tgt)
                     ((jl tgt) "  jl " ".lab" tgt)
                     ((jg tgt) "  jg " ".lab" tgt)
                     ((je tgt) "  je " ".lab" tgt)
                     ((jne tgt) "  jne " ".lab" tgt)
                     ((jmp tgt) "  jmp " ".lab" tgt)
                     ((ret) "  ret")
                     ((add dst src) "  add " dst ", " src)
                     ((sub dst src) "  sub " dst ", " src)
                     ((push val) "  push " val)
                     ((pop val) "  pop " val)
                     ((call name) "  call " name)
                     ((mem x) "[" x "]")
                     ((mem x p) "[" x "+" p "]")
                     ((dword x) "dword " x)
                     ((setbe x) "  setbe " x)
                     ((movzbl dst src) "  movzbl " dst ", " src)
                     ))

; modifiable: eax, ecx, edx
; registers: eax, ebx, ecx, edx, esp, ebp, esi, edi
; can use: eax, ebx, ecx, edx, ebp, esi, edi

(define (enumerate x)
  (define (enumerate-i x i)
    (if (empty? x)
        empty
        (cons (cons i (car x)) (enumerate-i (cdr x) (+ i 1)))))
  (enumerate-i x 0))

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

(define (flatten-labels-single enum-block)
  (cons (list 'local (car enum-block))
        (cdr enum-block)))

(define (flatten-labels func)
  (let ((base (first func)))
    (append* (cons (list (list 'label base))
                   (map
                    flatten-labels-single
                    (enumerate (cdddr func)))))))

(define (asm-write-format lookup vars format)
  (cond ((list? format) (string-append* (map (curry asm-write-format lookup vars) format)))
        ((string? format) format)
        ((symbol? format)
         (let ((lkup (assoc format vars)))
           (if lkup
               (asm-write-single lookup (second (assoc format vars)))
               (error "Nonexistent variable:" format 'in vars))))
        ((number? format) (number->string format))
        (else (error "Cannot format " (tree->string format)))))

(define (asm-write-single lookup inst)
  (cond ((string? inst) inst)
        ((symbol? inst) (symbol->string inst))
        ((number? inst) (number->string inst))
        ((not (pair? inst)) (error "Unhandlable type:" inst "with" lookup))
        ((eq? (car inst) 'seq) (asm-write-flat lookup (map (curry asm-write-single lookup) (cdr inst))))
        ((empty? lookup) (error (string-append "No output for: " (tree->string inst))))
        ((and (eq? (car inst) (caaar lookup)) (= (length inst) (length (caar lookup))))
         (asm-write-format lookup (zip (cdaar lookup) (cdr inst)) (cdar lookup)))
        (else (asm-write-single (cdr lookup) inst))))

(define (asm-write lookup tree)
  (asm-write-flat lookup (flatten-labels tree)))

(define (asm-write-flat lookup flattened)
  (string-join (filter (lambda (x) (not (= (string-length x) 0))) (map (curry asm-write-single lookup) flattened)) "\n"))

; (display (asm-write x86-output x86-simple-near))
(define medium (tree-transform (register-allocation x86-registers reduced) x86-output-prerules))
medium
(display (asm-write x86-output medium))
