#lang racket

(require "utilities.rkt")
(provide graph-color)

(define temp '((0 1 2 3 4 5 6 7 8 9 10 11 12)
               ((2 3) (eax 5) (1 7) (eax 9) (9 11) (eax 11))
               ((1 2) (1 3) (1 4) (1 5) (1 6) (5 6) (5 7) (5 8) (5 9) (5 10) (9 10))
               ((0 1) (2 3) (3 4) (1 7) (7 8) (5 11) (9 11) (11 12))))

(define (remap-one remaps x)
  (let ((lkp (assoc x remaps)))
    (if lkp (second lkp) x)))
(define (remap-nums remaps nums)
  (map (curry remap-one remaps) nums))
(define (remap remaps cmap)
  (map (curry remap-nums remaps) cmap))

(define (unmap remaps rmap)
  (define (unmap-get pair)
    (list (second pair) (second (assoc (car pair) rmap))))
  (sort (append rmap (map unmap-get remaps)) pair<?))

(define (num-sort x)
  (sort x <))

(define (deduplicate-known x)
  (define (get-dedup-i ts)
    (cond ((empty? (cdr ts)) (car ts))
          ((equal? (car ts) (cadr ts)) (get-dedup-i (cdr ts)))
          (else (print ts) (error "deduplicated to different register assignment"))))
  (define (get-dedup x)
    (cons (get-dedup-i (map car x)) (cdar x)))
  (let ((checks (unique (map second x) #:cmp< <)))
    (map (lambda (ci)
           (get-dedup (filter (lambda (xi)
                                (equal? (second xi) ci))
                              x)))
         checks)))

(define (ref<? a b)
  (if (symbol? a)
      (if (symbol? b)
          (symbol<? a b)
          #t)
      (if (symbol? b)
          #f
          (< a b))))

(define (pair<? a b)
  (if (equal? (car a) (car b))
      (ref<? (cadr a) (cadr b))
      (ref<? (car a) (car b))))

(define (graph-color targets forced conflict preference)
  (define-values (remaps known) (partition (lambda (x) (and (number? (car x)) (number? (cadr x)))) forced))
  (let* ((forward-remaps (map reverse remaps))
         (oriented-known (map (lambda (x) (if (number? (car x)) (reverse x) x)) known))
         (known-remapped (map reverse (deduplicate-known (remap forward-remaps known))))
         (remaining (without (unique (remap-nums forward-remaps targets) #:cmp< <) (map car known-remapped)))
         (conflicts (unique (map num-sort (remap forward-remaps conflict)) #:cmp< pair<?))
         (preferences (unique (map num-sort (filter (lambda (pair) (not (= (car pair) (cadr pair))))
                                          (remap forward-remaps preference)))
                    #:cmp< pair<?))
         (predef-regs (unique (map second known-remapped)))
         (forced-names (map (lambda (x) (list (car x) (cdr x))) (enumerate predef-regs)))
         (forced-names-rev (map reverse forced-names))
         (known-out (map (lambda (x) (list (first x) (second (assoc (second x) forced-names-rev)))) known-remapped)))
    (list forced-names
          (unmap remaps (graph-color-main
                         remaining
                         known-out
                         conflicts
                         preferences)))))
          
  ;(graph-color-noforce (remap known remaps) (remap conflict remaps) (remap preference remaps)))

(define (get-pair-matches mapping name)
  (filter-map (lambda (pair) (or (and (equal? (first pair) name) (second pair))
                                 (and (equal? (second pair) name) (first pair)))) mapping))

; requires a sorted list
(define (first-unused coll (i 0))
  (cond ((empty? coll) i)
        ((symbol? (car coll)) (first-unused (cdr coll) i))
        ((equal? i (car coll)) (first-unused (cdr coll) (+ i 1)))
        ((ref<? i (car coll)) i)))

(define (graph-color-main remaining known conflict preference)
  ;(trace 'main remaining known conflict preference)
  (define (get-pair-color other)
    (let ((pair (assoc other known)))
      (and pair (second pair))))
  (if (empty? remaining) known
      (let* ((myself (car remaining))
             (others (cdr remaining))
             (alloced (map car known))
             (conflicting-others (get-pair-matches conflict myself))
             (conflicting-colors (unique (filter-map get-pair-color conflicting-others) #:cmp< ref<?))
             (preference-others (get-pair-matches preference myself))
             (preference-colors (unique (filter-map get-pair-color preference-others) #:cmp< ref<?))
             (options (suffix (without-presorted preference-colors conflicting-colors #:cmp< ref<?) (first-unused conflicting-colors)))
             (new-color (car options)))
        ;(trace 'names myself others alloced)
        ;(trace 'confl conflicting-others conflicting-colors)
        ;(trace 'prefr preference-others preference-colors)
        ;(trace 'color options new-color)
        (graph-color-main others (cons (list myself new-color) known) conflict preference))))

;(cons 'A (apply graph-color temp))
