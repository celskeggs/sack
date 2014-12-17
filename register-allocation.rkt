#lang racket

(require "utilities.rkt")

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
        (cons (map cdr result) (register-allocation-apply block result))))))

(define (register-allocation regs tree)
  (let* ((allocated (map (curry register-allocation-block regs) (cdddr tree)))
         (allocated-blocks (map cdr allocated))
         (return-register regs)
         (allocated-used (without (unique (append* (map car allocated))) return-register)))
    (cons allocated-used
          (cons (car tree)
                (cons (cadr tree)
                      (cons (caddr tree)
                            allocated-blocks))))))

; ****

(provide register-allocate)

(define (register-allocate-block blk)
  (let ((swrs (cdr blk)))
    swrs))

(define (register-allocate f)
  (let ((name (first f)) (args (second f)) (ret (third f)) (blocks (cdddr f)))
    (map register-allocate-block blocks)))
