#lang racket

(provide full-swrify)

(require "utilities.rkt")
(require "boxdag.rkt")

(define sample-duplicated-set
  (make-boxdag '(logical/or
                 (generic/subresult
                  2
                  (x86/cmp/dd
                   (x86/movfm/d (get-reg ebp))
                   (x86/mov/c 1)))
                 (generic/subresult
                  1
                  (x86/cmp/dd
                   (x86/movfm/d (get-reg ebp))
                   (x86/mov/c 1))))))
(define sample-complex-set
  
   '((preserve85608
      .
      #&(x86/push/d
         #&(x86/movfm/d #&(x86/add/dc #&(get-reg ebp) 12))))
     (preserve85609 . #&(x86/call a))
     (preserve85610 . #&(x86/pop))
     (preserve85600
      .
      #&(x86/push/d #&(boxdag/preserve-ref preserve85609)))
     (preserve85605 . #&(x86/push/c 30))
     (preserve85606 . #&(x86/call b))
     (preserve85607 . #&(x86/pop))
     (preserve85601
      .
      #&(x86/push/d #&(boxdag/preserve-ref preserve85606)))
     (preserve85602 . #&(x86/call hello))
     (preserve85603 . #&(x86/pop))
     (preserve85604 . #&(x86/pop))
     (() . #&(boxdag/preserve-ref preserve85602))))
(define sample-preserve-set
  '(() . (return (x86/add/dd
                  (x86/movfm/d (x86/add/dd (get-reg ebp) (x86/mov/c 8)))
                  (x86/movfm/d (x86/add/dd (get-reg ebp) (x86/mov/c 12)))))))
(define sample-single-node
  '(return (x86/add/dd
            (x86/movfm/d (x86/add/dd (get-reg ebp) (x86/mov/c 8)))
            (x86/movfm/d (x86/add/dd (get-reg ebp) (x86/mov/c 12))))))


(struct swrified-reference (ref stmts) #:inspector #f)

(define (make-swrified-reference pair)
  (swrified-reference (car pair) (cdr pair)))
(define (get-swr)
  (box (void)))
(define (is-simple-ref x)
  (member (car x) '(get-reg)))
(define (swrify x (is-top-level #f)) ; returns ('swr swr-id) . ((swr . code) (swr . code) ...) OR x . empty
  (cond ((and (pair? x) (eq? (car x) 'boxdag/preserve-ref))
         (cons (second x) empty))
        ((and (pair? x) (not (is-simple-ref x)))
         (let ((gotten (swrify-node x)))
           (cons (list 'swr (car gotten)) (cdr gotten))))
        ((and (box? x) is-top-level)
         (swrify (unbox x)))
        ((and (box? x) (swrified-reference? (unbox x)))
         (cons (swrified-reference-ref (unbox x)) empty))
        ((box? x)
         (set-box! x (make-swrified-reference (swrify (unbox x))))
         (cons (swrified-reference-ref (unbox x)) (swrified-reference-stmts (unbox x))))
        (else (cons x empty))))
(define (swrify-all xes) ; returns (('swr swr-id) OR x ...) . ((swr . code) (swr . code) ...)
  (let ((processed (map swrify xes)))
    (cons (map car processed) (append* (map cdr processed)))))
(define (swrify-node x) ; returns swr-id (swr . code) (swr . code) ...
  (let* ((name (car x))
         (processed (swrify-all (cdr x)))
         (args (car processed))
         (stmts (cdr processed))
         (swr (get-swr)))
    (assert (symbol? name) "Expected a symbol head.")
    (cons swr (suffix stmts
                      (cons swr (cons name args))))))
(define (replace-all x replacements)
  (cond [(assoc x replacements) (cdr (assoc x replacements))]
        [(pair? x) (cons (replace-all (car x) replacements)
                         (replace-all (cdr x) replacements))]
        [(box? x) (set-box! x (replace-all (unbox x) replacements))
                  x]
        [else x]))
(define (swrify-multi-element x)
  (let ((swr-out (car x))
        (swrified (swrify (cdr x) #t)))
    (when (box? swr-out)
        (set-box! swr-out (car swrified)))
    swrified))
(define (swrify-multi x) ; returns swr-id (swr . code) (swr . code) ...
  (let ((targets (map car x)))
    (assert (= 1 (length (filter empty? targets))) "Should be exactly one result preserve!")
    (assert (empty? (last targets)) "The last preserve should be the result preserve!")
    (let ((replacements (map (lambda (target) (cons target (get-swr)))
                             (filter (lambda (x) (not (empty? x))) targets))))
      (let ((processed (map swrify-multi-element (replace-all x replacements))))
        (cons (car (last processed))
              (append* (map cdr processed)))))))

(define (assign-swr pair)
  (assert (void? (unbox (cadr pair))) "Expected swr to be unassigned.")
  (set-box! (cadr pair) (car pair)))
(define (assign-swrs x)
  (map assign-swr (enumerate (cdr x)))
  (strip-boxes x))

; Note that full-swrify will mangle the boxdag's contents.
(define (full-swrify x)
  (assign-swrs (swrify-multi x)))

;(full-swrify (get-boxdag-contents sample-duplicated-set))
;(full-swrify sample-complex-set)
