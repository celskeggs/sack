#lang racket

(require "utilities.rkt")

(provide calculate-reordering)

(define (obvious-movement next-num temp/save! temp/load temp/pop from to)
  (let* ((unique-from (unique from #:cmp< <))
         (used? (lambda (x) (member x to)))
         (next-num (lambda () (let ((n next-num))
                                (set! next-num (+ n 1))
                                n)))
         (temporary-locations (map (lambda (x) (list x (if (used? x)
                                                           (next-num)
                                                           (void)))) unique-from))
         (get-temp-loc (lambda (x) (second (assert (assoc x temporary-locations) "Unexpected: " x))))
         (saver (lambda (x)
                  (let ((sym (get-temp-loc x)))
                    (cons (gensym) ; a fake ssa because a real one isn't needed
                          (if (void? sym)
                              (list temp/pop)
                              (list temp/save! sym (list 'ssa x)))))))
         (loader (lambda (x) (cons x
                                   (list temp/load (get-temp-loc x))))))
    (append (map saver from)
            (map loader (reverse to)))))

(define (optimize actions)
  (if (or (empty? actions) (empty? (cdr actions))) actions
      (let ((cur (first actions))
            (next (second actions)))
        (if (and (eq? (car cur) 'stack-save!) (eq? (car next) 'stack-load) (equal? (second cur) (second next)))
            (optimize (cddr actions))
            (cons cur (optimize (cdr actions)))))))

(define (calculate-reordering first-num temp/save! temp/load temp/pop from to)
  (let* ((relevant (unique to #:cmp< <))
         (relevant-section (dropf-right from (lambda (x) (not (member x relevant))))))
    ; we need a sequence of swaps to make everything irrelevant be toward the bottom.
    (optimize (obvious-movement first-num temp/save! temp/load temp/pop relevant-section to))))

;(calculate-reordering 102 'tstk/slot! 'tstk/load 'tstk/pop '(3 4 2 1 8 9 9) '(1 3 2))
