#lang racket

(require "utilities.rkt")

(provide register-allocate)



(define (ssa-preferences block)
  (define (ssa-preferences-iter starts ends) ; looks through the list of starts and ends and finds places where one starts and another ends at the same time.
    (cond ((or (empty? starts) (empty? ends)) empty)
          ((< (cadar starts) (cadar ends)) (ssa-preferences-iter (cdr starts) ends))
          ((> (cadar starts) (cadar ends)) (ssa-preferences-iter starts (cdr ends)))
          (else ; we found a preference!
           (cons (sort (list (caar starts) (caar ends)) <)
                 (if (and (pair? (cdr ends)) (= (cadadr ends) (cadar starts)))
                     (ssa-preferences-iter starts (cdr ends))
                     (ssa-preferences-iter (cdr starts) ends))))))
  (let ((unzipped (unzip (map (lambda (x) (list (list (first x) (second x)) (list (first x) (third x)))) (ssa-usage-ranges block)))))
    (let ((sorted-starts (sort (first unzipped) (lambda (a b) (< (second a) (second b)))))
          (sorted-ends (sort (second unzipped) (lambda (a b) (< (second a) (second b))))))
      (ssa-preferences-iter sorted-starts sorted-ends))))

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
        (if (eq? (first tree) 'ssa) (list 'reg (cdr (assoc (second tree) lookup)))
            (map fix-tree tree))))
  (define (apply-line line)
    (if (eq? (first line) 'ssa)
        (cons 'reg (cons (cdr (assoc (second line) lookup))
                         (fix-tree (cddr line))))
        (cons (first line) (fix-tree (cdr line)))))
  (map apply-line block))

(define (register-allocate-block regs orig-block)
  (let ((block (cdr orig-block)))
    (let ((base-allocation (register-allocation-required regs block))
          (conflicts (ssa-conflicts block))
          (preferences (ssa-preferences block)))
      (let ((result (stream-first (color-solve (without (ssa-listing block) (map car base-allocation)) (cadr regs) conflicts preferences base-allocation))))
        (cons (map cdr result) (register-allocation-apply block result))))))

(define (register-allocate regs tree)
  (let* ((allocated (map (curry register-allocate-block regs) (cdddr tree)))
         (allocated-blocks (map cdr allocated))
         (return-register regs)
         (allocated-used (without (unique (append* (map car allocated))) return-register)))
    (cons allocated-used
          (cons (car tree)
                (cons (cadr tree)
                      (cons (caddr tree)
                            allocated-blocks))))))

(define e-regs '(eax ebx ecx edx esi edi))
(define e-oblk '((ssa 5)
   (0 x86/add/dc (get-reg ebp) 8)
   (1 x86/movfm/d (ssa 0))
   (2 x86/mov/c 2)
   (3 x86/cmp/dd (ssa 1) (ssa 2))
   (4 x86/jge (ssa 3) 2)
   (5 x86/jmp 1)))
(define e-blk (cdr e-oblk))
(register-allocate-block e-regs e-oblk)
