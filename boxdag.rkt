#lang racket

(require "utilities.rkt")

(provide strip-boxes strip-outer-boxes make-boxed add-element! make-boxdag get-boxdag-contents get-boxdag-exports
         boxdag-struct-preserved set-boxdag-struct-preserved! boxdag-struct-exported set-boxdag-struct-exported!)

(struct boxdag-struct
  (preserved ; ((uninterned-key-or-null . recursively-boxed-top-level) ...) list of side-effectful expressions. the null entry contains the value of the boxdag.
   exported) ; ((type key value) ...)
  #:mutable)

(define (get-boxdag-contents boxdag)
  (boxdag-struct-preserved boxdag))

(define (has-boxes expr)
  (or (box? expr) (and (pair? expr) (or (has-boxes (car expr)) (has-boxes (cdr expr))))))
(define (strip-boxes expr)
  (cond ((box? expr) (strip-boxes (unbox expr)))
        ((not (has-boxes expr)) expr) ; can only be a pair at this point if has-boxes is true
        (else (cons (strip-boxes (car expr)) (strip-boxes (cdr expr))))))
(define (strip-outer-boxes expr)
  (if (box? expr)
      (strip-outer-boxes (unbox expr))
      expr))
; Checks if any key matches the specified expression
(define (any-expr data-map stripped-expr)
  (and (not (empty? data-map))
       (or (equal? stripped-expr (strip-boxes (caar data-map)))
           (any-expr (cdr data-map) stripped-expr))))
; Assumes that the specified element is not in the map. Adds it to the map. elem: recursively-boxed expression, outermost layer optional. Returns: recursively-boxed expression.
(define (add-element! boxdag expr) ; expr may be box, return expr, boxed if necessary
  (if (box? expr)
      expr
      (box expr)))
; Get or make recursively-boxed expression for expr. expr: any expression, recursively-boxed or not or anywhere in-between.
(define (make-boxed expr)
  (box-recursively (strip-boxes expr)))
; Box recursively - given a completely unboxed argument.
(define (box-recursively x)
  (assert (not (box? x)))
  (if (pair? x)
      (box (map box-recursively x))
      x))
; Make a boxdag for expr.
(define (make-boxdag expr #:exported (exported empty))
  (boxdag-struct (list (cons null (make-boxed expr))) exported))
; Optimize the specified boxful tree. If can-strip is true, any outer boxes may be removed.
(define (optimize entry can-strip)
  (cond ((and (box? entry) can-strip)
         (optimize (unbox entry) #t))
        ((box? entry)
         (let ((optim (optimize (unbox entry) #t)))
           (if (equal? optim (unbox entry))
               entry
               (begin (set-box! entry optim)
                      entry))))
        ((pair? entry)
         (let ((head (optimize (car entry) #f))
               (tail (optimize (cdr entry) #f)))
           (if (and (equal? head (car entry))
                    (equal? tail (cdr entry)))
               entry
               (cons head tail))))
        (else entry)))
; Get a structured version of the exported data
(define (get-boxdag-exports boxdag)
  (let* ((all-elems (boxdag-struct-exported boxdag))
         (types (unique (map car all-elems) #:cmp< symbol<?))
         (type-enum (map (lambda (x) (cons (cdr x) (car x))) (enumerate types)))
         (typemap (make-vector (length types) empty)))
    (for/list ((elem all-elems))
      (let* ((typeid (cdr (assoc (car elem) type-enum)))
             (entry (cdr elem)))
        (vector-set! typemap typeid (cons entry (vector-ref typemap typeid)))))
    (zip types (vector->list typemap))))
