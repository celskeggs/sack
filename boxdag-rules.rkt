#lang racket

(require "utilities.rkt")
(require "boxdag.rkt")

(provide boxdag-rule apply-boxdag-rules-all boxdag-rule-args boxdag-rule-find boxdag-rule-repl)

(struct boxdag-rule
  (args   ; ((argument-name . predicate) ...)
   find
   repl)
  #:inspector #f)

; Find the predicate for the specified argument, or #f if not found.
(define (get-rule-predicate rule name)
  (let ((pair (assoc name (boxdag-rule-args rule))))
    (and pair (cdr pair))))
; Merge the two result maps. Both: ((key-symbol . value) ...).
(define (merge-sorted-match-results a b)
  (cond ((empty? a) b)
        ((empty? b) a)
        ((eq? (caar a) (caar b))
         (and (equal? (cdar a) (cdar b))
              (let ((subsort (merge-sorted-match-results (cdr a) (cdr b))))
                (and subsort (cons (car a) subsort)))))
        ((symbol<? (caar a) (caar b))
         (let ((subsort (merge-sorted-match-results (cdr a) b)))
           (and subsort (cons (car a) subsort))))
        (else
         (let ((subsort (merge-sorted-match-results a (cdr b))))
           (and subsort (cons (car b) subsort))))))
; Match the rule against the expression. returns: ((argname . value) (argname . value)) sorted by argname, or #f if no match.
(define (match-rule rule expression)
  (define (match-rule-recur finding element) ; TODO: check if the find is the same as the element, even with the boxes
    (cond ((box? element) (match-rule-recur finding (strip-outer-boxes element)))
          ((symbol? finding) ; symbol means either an argument or a required symbol
           (let ((predicate (get-rule-predicate rule finding)))
             (if predicate
                 (if (predicate element)
                     (list (cons finding element))
                     #f)
                 (if (eq? finding element)
                     empty
                     #f))))
          ((xor (pair? finding) (pair? element)) #f) ; either neither must be a pair or both must be
          ((not (pair? finding)) ; if they're not pairs, no recursion is necessary.
           (if (equal? finding element)
               empty
               #f))
        ; both find and element are pairs
        (else 
         (let ((head (match-rule-recur (car finding) (car element))))
           (if (not head)
               #f
               (let ((tail (match-rule-recur (cdr finding) (cdr element))))
                 (if (not (and head tail))
                     #f
                     (merge-sorted-match-results head tail))))))))
  (match-rule-recur (boxdag-rule-find rule) expression))
; Replace the specified variables with their values in the expression. vars: ((varname . value) ...)
(define (apply-replacements vars in)
  (assert (not (box? in)) "Cannot have boxes in replacements!")
  (cond ((pair? in) (cons (apply-replacements vars (car in)) (apply-replacements vars (cdr in))))
        ((procedure? in) (in vars))
        (else (let ((found (assoc in vars)))
                (if found
                    (cdr found)
                    in)))))
; Get and sort the variable names from vars. vars: ((varname . value) ...)
(define (get-sorted-var-names vars)
  (sort (map car vars) symbol<?))
; Generate the replacement for the rule for element.
(define (replace-rule rule element)
  (let ((vars (match-rule rule element)))
    (assert (equal? (get-sorted-var-names vars) (get-sorted-var-names (boxdag-rule-args rule))) "Failed to match all arguments in boxdag rule.")
    (apply-replacements vars (boxdag-rule-repl rule))))
; Apply rule to the boxdag, just once.
(define (apply-boxdag-rule-once rule boxdag)
  (let ((applicable-to (findf
                             (curry match-rule rule)
                             (get-data-boxes boxdag))))
    (if applicable-to
        (let ((calculated (get-boxed! boxdag (replace-rule rule applicable-to))))
          (assert (not (equal? (strip-boxes calculated) (strip-boxes applicable-to))) "Expected replacer to modify the element!")
          (set-box! applicable-to calculated)
          (let ((lookup (get-boxdag-element-pair boxdag calculated)))
            (if lookup
                (set-box! applicable-to (cdr lookup))
                (add-element! boxdag applicable-to)))
          #t)
        #f)))
; Apply the first applicable rule to the boxdag, just once.
(define (apply-boxdag-rules-once rules boxdag)
  (and (not (empty? rules))
       (or (apply-boxdag-rule-once (car rules) boxdag)
           (apply-boxdag-rules-once (cdr rules) boxdag))))
; Split into a series of preserved expressions.
(define (apply-individual-preservation boxdag entry)
  (define (eventually-contains-preserve x)
    (or (equal? x 'boxdag/preserve)
        (and (pair? x)
             (or (eventually-contains-preserve (car x))
                 (eventually-contains-preserve (cdr x))))
        (and (box? x)
             (eventually-contains-preserve (unbox x)))))
  (define output (box empty))
  (define (append-output key value) ; prepends to output, which is later reversed
    (set-box! output (cons (cons key value) (unbox output)))
    key)
  (define (recurse x)
    (cond ((and (pair? x) (eq? (car x) 'boxdag/preserve) (not (empty? (cdr x))) (empty? (cddr x)))
           (list 'boxdag/preserve-ref (append-output (gensym 'preserve) (second x))))
          ((pair? x)
           (let ((head (recurse (car x))))
             (cons head (recurse (cdr x)))))
          ((box? x)
           (recurse (unbox x)))
          (else x)))
  (if (not (eventually-contains-preserve entry))
      (list entry)
      (begin
        (append-output (car entry) (make-boxed! boxdag (recurse (cdr entry))))
        (reverse (unbox output)))))
; Move backward any preserved expressions.
(define (apply-preservation boxdag)
  (let ((orig-length (length (boxdag-struct-preserved boxdag))))
    (set-boxdag-struct-preserved! boxdag (append* (map (curry apply-individual-preservation boxdag) (boxdag-struct-preserved boxdag))))
    (not (= orig-length (length (boxdag-struct-preserved boxdag))))))
; Apply any applicable rules and external processors to the boxdag, as many times as possible.
(define (apply-boxdag-rules-all rules boxdag)
  (optimize-boxdag boxdag)
  (and (or (apply-preservation boxdag)
           (apply-boxdag-rules-once rules boxdag))
       (apply-boxdag-rules-all rules boxdag)))
