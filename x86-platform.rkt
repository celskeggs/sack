#lang racket

(define sample '(math (u4 u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (arg 1))
                       (swr 2 (+ (swr 0) (swr 1)))
                       (return (swr 2)))))

; x86 information
'(platform x86
          (registers eax ebx ecx edx esi edi)
          (return-behavior returned-value
                           (set-register eax returned-value))
          (argument-behavior argnum
                             (get-memory (+ (get-register ebp) (+ 8 (* argnum 4)))))
          (instruction-delimiter "\n")
          (definition-syntax "global " name "\n" name ":")
          (jumptarget-syntax ".lab" name ":")
          (instructions
           (inst (movfm (register dest) (register source))
                 ("  mov " dest ", [" source "]")
                 (behavior
                  (set-register dest (get-memory source)))
                 (stack
                  (push (get-memory (pop))))
                 )
           (inst (add dest source)
                 ("  add " dest ", " source)
                 (behavior
                  (set-register dest (+ (get-register source) (get-register dest))))
                 (stack
                  (push (+ (pop) (pop))))
                 (substitute
                  
                  )
                 )
           (inst (mov (register dest) (register source))
                 ("  mov " dest ", " source)
                 (behavior
                  (set-register dest (get-register source)))
                 (stack
                  (push (pop)))
                 )
           (inst (movcst (register dest) (constant source))
                 ("  mov " dest ", " source)
                 (behavior
                  (set-register dest source))
                 (stack
                  (push source)))
           ))

'(reduction-behavior
 (replace (* (number a) (number b))
          (lambda (a b) (* a b)))
 (replace (+ (number a) (number b))
          (lambda (a b) (+ a b))))

(define reduced '((return (+ (arg 0) (arg 1)))
                  (set-register eax (+ (arg 0) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) (+ 8 (* 0 4)))) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) (+ 8 0))) (arg 1)))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) (+ 8 (* 1 4))))))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) (+ 8 4)))))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) 12))))
                  (set-register eax (+ (get-memory (+ (get-register ebp) 8)) (get-memory (+ (get-register ebp) 12))))
                  (push (+ (get-memory (+ (pop) 8)) (get-memory (+ (pop) 12))))
                  (push (+ (get-memory (+ (pop) (movcst 8))) (get-memory (+ (pop) (movcst 12)))))
                  (push (+ (get-memory (add (pop) (movcst 8))) (get-memory (add (pop) (movcst 12)))))
                  (push (+ (movfm (add (pop) (movcst 8))) (movfm (add (pop) (movcst 12)))))
                  (push (add (movfm (add (pop) (movcst 8))) (movfm (add (pop) (movcst 12)))))
                  ))

(define (has-boxes expr)
  (or (box? expr) (and (pair? expr) (or (has-boxes (car expr)) (has-boxes (cdr expr))))))
(define (strip-boxes expr)
  (cond ((box? expr) (strip-boxes (unbox expr)))
        ((not (has-boxes expr)) expr) ; can only be a pair at this point if has-boxes is true
        (else (cons (strip-boxes (car expr)) (strip-boxes (cdr expr))))))
(define (add-boxdag-elem context elem) ; elem is a box, return elem
  (set-box! context (cons (cons (unbox elem) elem) (unbox context)))
  elem)
(define (make-boxdag-i context expr)
  ; context: #&((list . box-for-list) (list . box-for-list) ...)
  ; returns: boxed-expr
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (assoc (strip-boxes expr) (unbox context))))
           (if found
               (cdr found)
               (add-boxdag-elem context (box (map (curry make-boxdag-i context) expr))))))
        (else expr)))
(define (make-boxdag expr)
  (let ((context (box empty)))
    (cons context (make-boxdag-i context expr))))
(define (make-boxdag-contents context expr)
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (assoc (strip-boxes expr) (unbox context))))
           (if found
               (cdr found)
               (map (curry make-boxdag-i context) expr))))
        (else expr)))
(define (get-boxdag-map boxdag)
  (unbox (car boxdag)))
(define (get-boxdag-boxes boxdag)
  (map cdr (get-boxdag-map boxdag)))
(define (get-boxdag-content boxdag)
  (cdr boxdag))
(define (merge-sorted-match-results a b)
  (cond ((eq? (caar a) (caar b))
         (and (equal? (cdar a) (cdar b))
              (let ((subsort (merge-sorted-match-results (cdr a) (cdr b))))
                (and subsort (cons (car a) subsort)))))
        ((symbol<? (caar a) (caar b))
         (let ((subsort (merge-sorted-match-results (cdr a) b)))
           (and subsort (cons (car a) subsort))))
        (else
         (let ((subsort (merge-sorted-match-results a (cdr b))))
           (and subsort (cons (car b) subsort))))))
(define (merge-match-results a b)
  (if (not (and a b))
      #f
      (merge-sorted-match-results a b)))
; argument args: ((arg . predicate-or-null) (arg . predicate-or-null) ...)
; returns: ((argname . value) (argname . value)) sorted by argname
(define (match-boxdag-rule args find element) ; TODO: check if the find is the same as the element, even with the boxes
  (cond ((box? element) (match-boxdag-rule find (unbox element)))
        ((symbol? find)
         (let ((lookup (assoc find args)))
           (cond ((not lookup) (and (eq? find element) empty))
                 ((or (null? (cdr lookup))
                      ((cdr lookup) element))
                  (list (cons find element)))
                 (else #f))))
        ((xor (pair? find) (pair? element)) #f) ; either neither must be a pair or both must be
        ((not (pair? find)) (and (equal? find element) empty))
        ; both find and element are pairs
        (else (merge-match-results (match-boxdag-rule args (car find) (car element))
                                   (match-boxdag-rule args (cdr find) (cdr element))))))
(define (replace-boxdag-rule args find replace element)
  )
(define (apply-boxdag-rule-once args find replace boxdag)
  (let ((found (findf
                (curry match-boxdag-rule args find)
                (get-boxdag-boxes boxdag))))
    (if found
        (let ((calculated (make-boxdag-contents boxdag (replace-boxdag-rule args find replace (unbox found)))))
          (assert (not (equal? (strip-boxes calculated) (strip-boxes found))) "Expected replacer to modify the element!")
          (set-box! found calculated)
          (add-boxdag-elem boxdag found) ; WHAT IF: the element is already in boxdag? we should nest the boxes.
          #t)
        #f)))

(define reduced2 '(((swr 0 (arg 0))
                   (swr 1 (arg 1))
                   (swr 2 (+ (swr 0) (swr 1)))
                   (return (swr 2)))
                  ((swr 0 (get-memory (+ (reg ebp) (+ 8 (* 0 4)))))
                   (swr 1 (get-memory (+ (reg ebp) (+ 8 (* 1 4)))))
                   (swr 2 (+ (swr 0) (swr 1)))
                   (return (swr 2)))
                  ((swr 3 (+ (reg ebp) (+ 8 (* 0 4))))
                   (swr 0 (get-memory (swr 3)))
                   (swr 1 (get-memory (+ (reg ebp) (+ 8 (* 1 4)))))
                   (swr 2 (+ (swr 0) (swr 1)))
                   (return (swr 2)))
                  ((swr 6 (* (cst 0) (cst 4)))
                   (swr 5 (+ (cst 8) (swr 6)))
                   (swr 4 (reg ebp))
                   (swr 3 (+ (swr 4) (swr 5)))
                   (swr 0 (get-memory (swr 3)))
                   (swr 7 (* (cst 1) (cst 4)))
                   (swr 8 (+ (cst 8) (swr 7)))
                   (swr 9 (+ (swr 4) (swr 8)))
                   (swr 1 (get-memory (swr 9)))
                   (swr 2 (+ (swr 0) (swr 1)))
                   (reg eax (swr 2)))
                  ((14 (cst 0))
                   (12 (cst 4))
                   (6 (* (14) (12)))
                   (13 (cst 8))
                   (5 (+ (13) (6)))
                   (4 (ebp))
                   (3 (+ (4) (5)))
                   (0 (get-memory (3)))
                   (11 (cst 1))
                   (7 (* (11) (12)))
                   (10 (cst 8))
                   (8 (+ (10) (7)))
                   (9 (+ (4) (8)))
                   (1 (get-memory (9)))
                   (2 (+ (0) (1)))
                   (eax (2)))))