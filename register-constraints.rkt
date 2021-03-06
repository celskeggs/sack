#lang racket

(require "utilities.rkt")
(require "rule-generator.rkt")
(require "platform-structures.rkt")

(provide register-constrain instructions-argify)

; Issue is a range (a b) where the endpoints invariably conflict in register allocation
; To fix this, we assign the former a new SSA in between a and b and copy into it.
; But we also need to do the same at the end - and move it back, because otherwise it might not solve the conflict.
(define (mitigate block issue remap-op)
  (define has-hit-instance #f) ; TODO: don't use mutable state
  (define remap-target (+ 1 (car issue))) ; move away here
  (define remap-source (+ 1 (caar (filter (lambda (line) (member (list 'ssa (car issue)) (cdr line))) (cdr block))))) ; move back here
  (define two-replaces (not (= (+ 1 remap-target) remap-source)))
  (assert (< remap-target remap-source) "Bad ordering of accesses!")
  (define (remap-id ssa-id)
    (if (<= ssa-id (car issue)) ssa-id
        (if (or (< ssa-id (- remap-source 1)) (not two-replaces)) (+ 1 ssa-id) (+ 2 ssa-id))))
  (define (remap-arg line-ssa arg)
    (cond ((and (pair? arg) (eq? (car arg) 'ssa))
           (if (and line-ssa (not has-hit-instance) (= (cadr arg) (car issue)))
               (begin
                 (set! has-hit-instance #t)
                 (cons 'ssa (cons (if two-replaces remap-source remap-target) (cddr arg))))
               (cons 'ssa (cons (remap-id (cadr arg)) (cddr arg)))))
          ((and (pair? arg) (eq? (car arg) 'generic/subresult))
           (map (curry remap-arg line-ssa) arg))
          (else arg)))
  (define (remap-line line)
    (cons (remap-id (car line)) (map (curry remap-arg (car line)) (cdr line))))
  (define (insert-ssa block entry)
    (if (< (caar block) (car entry))
        (cons (car block) (insert-ssa (cdr block) entry))
        (cons entry block)))
  (cons (remap-arg #f (car block))
        (let* ((to-insert (list remap-target remap-op (list 'ssa (car issue))))
               (inserted (insert-ssa (map remap-line (cdr block)) to-insert)))
          (if two-replaces
              (insert-ssa
               inserted
               (list remap-source remap-op (list 'ssa remap-target)))
              inserted))))

(define (instructions-argify platform code)
  (define (get-instruction name)
    (car (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs platform))))
  (define (remove-get-reg expr)
    (if (and (pair? expr) (eq? (first expr) 'get-reg))
        (second expr)
        expr))
  (define (single-argify line)
    (let* ((out-regs (car line))
           (name (cadr line))
           (arguments (cddr line))
           (instr (get-instruction name))
           (i-args (instruction-struct-arguments instr))
           (i-rets (instruction-struct-returns instr))
           (remain-args (filter (lambda (x) (not (member (car x) i-rets))) i-args))
           (cstrts (instruction-struct-constraints instr))
           (amap (zip (map car (instruction-struct-used-arguments instr)) arguments))
           (rmap (cond ((empty? i-rets) empty)
                       ((empty? (cdr i-rets)) (list (list (car i-rets) (list 'get-reg out-regs))))
                       (else (zip i-rets (map-curry list 'get-reg out-regs)))))
           (tmap (map-join amap rmap)))
      (assert (= (length (unique i-rets)) (length i-rets)) "bad i-rets")
      (assert (= (- (length i-args) (length remain-args)) (length i-rets)) "invalid argset")
      ;(trace 'ARGIFY-1 out-regs name arguments)
      ;(trace 'ARGIFY-2 i-args i-rets remain-args)
      ;(trace 'ARGIFY-3 cstrts amap rmap)
      ;(trace 'ARGIFY i-args tmap)
      (assert (equal? (sort (map car i-args) symbol<?)
                      (sort (map car tmap) symbol<?)) "Key mismatch!")
      (cons name (map (lambda (pair) (list (first pair) (remove-get-reg (second pair)))) tmap))));(map (lambda (key) (remove-get-reg (second (assoc (car key) tmap)))) i-args))))
  (define (block-argify block)
    (map single-argify block))
  (map block-argify code))

(define (register-constrain platform code #:is-single (is-single #f))
  (define (get-instruction name)
    (let ((matches (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs platform))))
      (assert (= (length matches) 1) (string-append "Expected exactly one instruction for name: " (symbol->string name)))
      (car matches)))
  (define (get-elem k m)
    (let ((found (assoc k m)))
      (and found (cdr found))))
  (define (single-forces line)
    (let* ((out-ssa (car line))
           (name (cadr line))
           (arguments (cddr line))
           (instr (get-instruction name))
           (i-args (instruction-struct-arguments instr))
           (i-rets (instruction-struct-returns instr))
           (remain-args (filter (lambda (x) (not (member (car x) i-rets))) i-args))
           (cstrts (instruction-struct-constraints instr))
           (amap (zip (map car (instruction-struct-used-arguments instr)) arguments))
           (pmap (filter-map (lambda (x) (and (and (pair? (cdr x)) (eq? (cadr x) 'ssa)) (cons (car x) (caddr x)))) (map (curry apply cons) amap))))
      (assert (= (length (unique i-rets)) (length i-rets)) "bad i-rets")
      (assert (= (- (length i-args) (length remain-args)) (length i-rets)) "invalid argset")
      (append
       (filter car (map (lambda (argp) (list (get-elem (car argp) cstrts) (cdr argp))) pmap))
       (let ((rets (instruction-struct-returns instr)))
         (if (= (length rets) 1)
             (filter car (list (list (or (get-elem (car rets) cstrts) (get-elem (car rets) pmap)) out-ssa)))
             (map (lambda (reti) (list (or (get-elem (cdr reti) cstrts) (get-elem (cdr reti) pmap)) (list out-ssa (car reti)))) (enumerate rets)))))))
  (define (ssa-forces blk)
    (append* (map single-forces (cdr blk))))
  (define (ssa-end blk ssa)
    (let ((uses (filter-map (lambda (line) (and (member (list 'ssa ssa) line) (car line))) blk)))
      (and (not (empty? uses)) (apply max uses))))
  (define (ssa-range blk ssa)
    (let ((end (ssa-end blk ssa)))
      (list ssa (or end ssa))))
  (define (ssa-conflicts ranges)
    (if (empty? ranges) empty
        (let ((start (caar ranges))
              (end (cadar ranges)))
          (append (filter-map (lambda (x) (and (< (car x) end) (list start (car x)))) (cdr ranges))
                  (ssa-conflicts (cdr ranges))))))
  (define (ssa-interferences blk)
    (let* ((ssas (map car (cdr blk)))
           (ranges (filter-map (curry ssa-range (cdr blk)) ssas)))
      (ssa-conflicts ranges)))
  (define (ssa-preferences blk)
    (let* ((ssas (map car (cdr blk)))
           (ranges (filter-map (curry ssa-range (cdr blk)) ssas)))
      (append-map (lambda (x) (filter-map (lambda (e) (and (= x (second e)) (not (= x (first e))) (list (first e) x))) ranges)) (map car ranges))))
  (define (ref<? a b)
    (cond ((and (empty? a) (empty? b)) #f)
          ((symbol? a) (cond ((symbol? b) (symbol<? a b))
                             ((number? b) #t)
                             ((pair? b) #t)))
          ((number? a) (cond ((symbol? b) #f)
                             ((number? b) (< a b))
                             ((pair? b) #t)))
          ((pair? a) (cond ((symbol? b) #f)
                           ((number? b) #f)
                           ((pair? b) (if (equal? (car a) (car b))
                                          (ref<? (cdr a) (cdr b))
                                          (ref<? (car a) (car b))))))))
  (define (find-force-blob forces ref)
    (let iter ((done empty) (complete empty) (remaining (list ref)))
      ;(trace 'ITER done remaining)
      (if (empty? remaining)
          (filter-map (lambda (elem) (and (not (equal? ref elem))
                                          (sort (list ref elem) ref<?))) done)
          (let* ((active (car remaining))
                 (rest (cdr remaining))
                 (matches-first (filter-map (lambda (o) (and (equal? active (first o)) (second o))) forces))
                 (matches-second (filter-map (lambda (o) (and (equal? active (second o)) (first o))) forces))
                 (matches (unique (append done matches-first matches-second) #:cmp< ref<?)))
            (iter matches (cons active complete) (without (append rest matches) (cons active complete)))))))
  (define (expand-forces forces)
    ;(trace 'expand-forces forces)
    (let* ((to-expand-from (unique (append (map first forces) (map second forces)) #:cmp< ref<?))
           (each-expanded (map-curry find-force-blob forces to-expand-from))
           (all-expanded (append* each-expanded)))
      ;(trace 'expand-forces to-expand-from each-expanded all-expanded)
      (unique all-expanded #:cmp< ref<?)))
  (define (tuplify-id block conf)
    (let* ((name (second (assoc conf block)))
           (rets (instruction-struct-returns (get-instruction name))))
      (if (<= (length rets) 1)
          (list conf)
          (cons conf (map-curry list conf (range 0 (length rets)))))))
  (define (tuplify-pair block conf)
    (let ((firsts (tuplify-id block (first conf)))
          (seconds (tuplify-id block (second conf))))
      (append* (map (lambda (x) (map-curry list x seconds)) firsts))))
  (define (tuplify-interferences block conf)
    (append* (map-curry tuplify-pair block conf)))
  (define (constrain-once block)
    (let* ((forces-initial (ssa-forces block))
           (forces-expanded (expand-forces forces-initial)))
      (list (expand-forces (ssa-forces block))
            (tuplify-interferences block (ssa-interferences block))
            (ssa-preferences block))))
  (define (constrain-issues constraints)
    (let ((forces (first constraints))
          (interferences (second constraints)))
      (filter (lambda (x) (member x interferences)) forces)))
  (define (constrain-bad? constraints)
    (not (empty? (constrain-issues constraints))))
  (define (constrain-loop block)
    (let ((constraints (constrain-once block)))
      (if (constrain-bad? constraints)
          (constrain-loop (mitigate block (car (constrain-issues constraints)) (platform-struct-reg-remap-op platform)))
          (list block constraints))))
  (if is-single
      (constrain-loop code)
      (map constrain-loop code)))
