#lang racket

(require "utilities.rkt")
(require "boxdag.rkt")

(provide boxdag-rule apply-boxdag-rules-all boxdag-rule-args boxdag-rule-find boxdag-rule-repl fixup-boxdag-preserves)

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
           (if (eq? finding '_)
               empty
               (let ((predicate (get-rule-predicate rule finding)))
                 (if predicate
                     (if (predicate element)
                         (list (cons finding element))
                         #f)
                     (if (eq? finding element)
                         empty
                         #f)))))
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
; Find matching subtree
(define (search-tree rule tree)
  (cond ((pair? tree)
         (or (search-tree rule (car tree))
             (search-tree rule (cdr tree))))
        ((box? tree)
         (or (and (match-rule rule tree) tree)
             (search-tree rule (unbox tree))))
        (else #f)))
; Find matching subtree of list of preserves
(define (search-preserve-list rule tree)
  (and (not (empty? tree))
       (or (search-tree rule (car tree))
           (search-preserve-list rule (cdr tree)))))
; Find matching subtree of boxdag
(define (search-boxdag rule boxdag)
  (search-preserve-list rule (boxdag-struct-preserved boxdag)))
; Replace the specified variables with their values in the expression. vars: ((varname . value) ...)
(define (apply-replacements vars cur-exports in)
  (assert (not (box? in)) "Cannot have boxes in replacements!")
  (cond ((pair? in) (cons (apply-replacements vars cur-exports (car in)) (apply-replacements vars cur-exports (cdr in))))
        ((procedure? in) (in vars cur-exports))
        (else (let ((found (assoc in vars)))
                (if found
                    (cdr found)
                    in)))))
; Get and sort the variable names from vars. vars: ((varname . value) ...)
(define (get-sorted-var-names vars)
  (sort (map car vars) symbol<?))
; Generate the replacement for the rule for element.
(define (replace-rule rule cur-exports element)
  (let ((vars (match-rule rule element)))
    (assert (equal? (get-sorted-var-names vars) (get-sorted-var-names (boxdag-rule-args rule))) "Failed to match all arguments in boxdag rule.")
    (apply-replacements vars cur-exports (boxdag-rule-repl rule))))
; Apply rule to the boxdag, just once.
(provide replace-rule match-rule apply-replacements)
(define (apply-boxdag-rule-once rule cur-exports boxdag)
  (let ((applicable-to (search-boxdag rule boxdag)))
    ;(trace 'applicable-to applicable-to (get-data-boxes boxdag))
    (if applicable-to
        (let ((calculated (unbox (make-boxed (replace-rule rule cur-exports applicable-to)))))
          ;(trace 'APPLY-RULE rule)
          (assert (not (equal? (strip-boxes calculated) (strip-boxes applicable-to))) "Expected replacer to modify the element!")
          (set-box! applicable-to calculated)
          #t)
        #f)))
; Apply the first applicable rule to the boxdag, just once.
(define (apply-boxdag-rules-once rules cur-export boxdag)
  (and (not (empty? rules))
       (or (apply-boxdag-rule-once (car rules) cur-export boxdag)
           (apply-boxdag-rules-once (cdr rules) cur-export boxdag))))
(struct unready-preserve (value) #:mutable #:inspector #f)
; Split into a series of preserved expressions.
(define (apply-individual-preservation boxdag entry #:avoid-preserve avoid-for)
  (define (eventually-contains-preserve x)
    (or (member x '(boxdag/preserve boxdag/preserve-ref-prepared boxdag/preserve-immediate boxdag/export))
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
    (cond ((and (pair? x) (member (car x) '(boxdag/preserve boxdag/preserve-immediate)) (not (empty? (cdr x))) (empty? (cddr x)))
           (if (and (member (car (unbox (second x))) avoid-for) (not (eq? (car x) 'boxdag/preserve-immediate)))
               (let ((completee (unready-preserve (gensym 'preserve))))
                 (append-output (unready-preserve-value completee) completee)
                 (list 'boxdag/preserve-ref-prepared completee (second x)))
               (list 'boxdag/preserve-ref (append-output (gensym 'preserve) (second x)))))
          ((and (pair? x) (eq? (car x) 'boxdag/preserve-ref-prepared-immediate))
           (begin0 (list 'boxdag/preserve-ref (unready-preserve-value (second x)))
                   (set-unready-preserve-value! (second x) (make-boxed (third x)))))
          ((and (pair? x) (eq? (car x) 'boxdag/export))
           (set-boxdag-struct-exported! boxdag (cons (cdr x) (boxdag-struct-exported boxdag)))
           (third x))
          ((pair? x)
           (let ((head (recurse (car x))))
             (cons head (recurse (cdr x)))))
          ((box? x)
           (recurse (unbox x)))
          (else x)))
  (if (not (eventually-contains-preserve entry))
      (list entry)
      (begin
        (append-output (car entry) (make-boxed (recurse (cdr entry))))
        (reverse (unbox output)))))
(define (apply-individual-deferred-preservation boxdag entry any-found)
  (cons (car entry)
        (make-boxed
         (let recurse ((x (cdr entry)))
           (cond ((and (pair? x) (eq? (car x) 'boxdag/preserve-ref-prepared))
                  (begin0 (list 'boxdag/preserve-ref (unready-preserve-value (second x)))
                          (set-box! any-found #t)
                          (set-unready-preserve-value! (second x) (make-boxed (third x)))))
                 ((pair? x)
                  (cons (recurse (car x)) (recurse (cdr x))))
                 ((box? x)
                  (recurse (unbox x)))
                 (else x))))))
(define (fixup-boxdag-preserves boxdag)
  (let ((any-found #f))
    (set-boxdag-struct-preserved!
     boxdag
     (map (lambda (pres) (if (unready-preserve? (cdr pres))
                             (begin0 
                               (cons (car pres) (unready-preserve-value (cdr pres)))
                               (set! any-found #t))
                             pres))
          (boxdag-struct-preserved boxdag)))
    any-found))
(define (apply-deferred-preservation boxdag)
  (let ((any-found (box #f)))
    (set-boxdag-struct-preserved! boxdag (map (lambda (pres) (apply-individual-deferred-preservation boxdag pres any-found)) (boxdag-struct-preserved boxdag)))
    ;(trace 'result any-found)
    (unbox any-found)))
; Move backward any preserved expressions.
(define (apply-preservation boxdag #:avoid-preserve avoid-for)
  (let ((orig-length (length (boxdag-struct-preserved boxdag))) (orig-export-length (length (boxdag-struct-exported boxdag))))
    (set-boxdag-struct-preserved! boxdag (append* (map (lambda (pres) (apply-individual-preservation boxdag pres #:avoid-preserve avoid-for)) (boxdag-struct-preserved boxdag))))
    (not (and (= orig-length (length (boxdag-struct-preserved boxdag))) (= orig-export-length (length (boxdag-struct-exported boxdag)))))))
; Apply any applicable rules and external processors to the boxdag, as many times as possible.
(define (apply-boxdag-rules-all rules boxdag #:avoid-preserve avoid-for #:hooks (hooks empty))
  ;(trace 'cycle (get-boxdag-contents boxdag))
  (and (or (apply-preservation boxdag #:avoid-preserve avoid-for)
           (apply-boxdag-rules-once rules (get-boxdag-exports boxdag) boxdag)
           (apply-deferred-preservation boxdag)
           (fixup-boxdag-preserves boxdag)
           (run-all-hooks hooks boxdag))
       (apply-boxdag-rules-all rules boxdag #:avoid-preserve avoid-for #:hooks hooks)))
(define (run-all-hooks hooks boxdag)
  (if (empty? hooks) #f
      (or ((car hooks) boxdag)
          (run-all-hooks (cdr hooks) boxdag))))
