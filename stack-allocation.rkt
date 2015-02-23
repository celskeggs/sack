#lang racket

(require "utilities.rkt")
(require "platform-structures.rkt")
(require "rule-generator.rkt")

(provide stack-allocate stack-make-assembly stack-deepest-stack)

(define (stack-allocate platform ssa-assembly)
  (map-curry calculate-stacks platform ssa-assembly))

(define (get-instruction platform name)
  (let ((matches (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs platform))))
    (assert (= (length matches) 1) (string-append "Expected exactly one instruction for name: " (symbol->string name)))
    (car matches)))

(define (get-returns platform line)
  (let* ((ssa-id (first line))
         (instruction-name (second line))
         (instruction (get-instruction platform instruction-name))
         (counts (length (instruction-struct-returns instruction))))
    (assert (<= counts 1) "Multiple stack-based returns not yet supported.")
    (if (= counts 1)
        (list (list 'ssa ssa-id))
        empty)))

(define (get-ssa arg)
  (and (pair? arg) (eq? (first arg) 'ssa) arg))
(define (get-nonssa arg)
  (and (not (and (pair? arg) (eq? (first arg) 'ssa))) arg))

(define (evaluate-stacks platform lines stack) ; stack is stored with top element first, unlike the rest of the system.
  (if (empty? lines)
      (begin
        (assert (empty? stack) "Stack should be empty at the end!")
        empty)
      (let* ((line (car lines))
             (arguments (filter-map get-ssa (cddr line)))
             (remain-args (map get-nonssa (cddr line)))
             (avail-arguments (reverse (take stack (length arguments))))
             (remain-stack (list-tail stack (length arguments)))
             (returns (get-returns platform line)))
        (assert (equal? avail-arguments arguments) "Stack mismatch... TODO")
        (cons (list (cons (second line) remain-args) 'STK remain-stack 'ARGS arguments)
              (evaluate-stacks platform (cdr lines)
                               (append (reverse returns) remain-stack))))))
  
(define (calculate-stacks platform ssa-assembly)
  (evaluate-stacks platform (cdr ssa-assembly) empty))

(define (stack-make-assembly-for-block platform sequence)
  (let* ((lines (map car sequence))
         (instr-refs (map car lines))
         (instrs (map-curry get-instruction platform instr-refs))
         (instr-args (map (lambda (x) (map car (instruction-struct-arguments x))) instrs))
         (argses (map cdr lines))
         (mappings (map zip instr-args argses)))
    (map cons instr-refs mappings)))

(define (stack-make-assembly platform stack-processed)
  (map-curry stack-make-assembly-for-block platform stack-processed))

(define (stack-deepest-line line)
  (assert (eq? (second line) 'STK))
  (length (third line)))
(define (stack-deepest-block block)
  (apply max (map stack-deepest-line block)))
(define (stack-deepest-stack stack-processed)
  (apply max (map stack-deepest-block stack-processed)))
