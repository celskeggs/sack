#lang racket

(require "utilities.rkt")

(provide strip-boxes strip-outer-boxes get-data-boxes get-boxed! make-boxed! add-element! make-boxdag get-boxdag-contents get-boxdag-exports
         get-boxdag-element-pair boxdag-struct-preserved set-boxdag-struct-preserved! boxdag-struct-exported set-boxdag-struct-exported! get-data-map optimize-boxdag)

(struct boxdag-struct
  (data-map  ; ((unboxed . recursively-boxed) (unboxed . recursively-boxed) ...)
   preserved ; ((uninterned-key-or-null . recursively-boxed-top-level) ...) list of side-effectful expressions. the null entry contains the value of the boxdag.
   exported) ; ((type key value) ...)
  #:mutable)

(define (get-data-map boxdag)
  (boxdag-struct-data-map boxdag))
(define (set-data-map! boxdag updated)
  (set-boxdag-struct-data-map! boxdag updated))
(define (get-data-boxes boxdag)
  (map cdr (get-data-map boxdag)))
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
; Prepends entry to the boxdag's map. entry: (unboxed-element . boxed-element)
(define (add-entry! boxdag entry)
  (set-data-map! boxdag (cons entry (get-data-map boxdag))))
; Checks if any key matches the specified expression
(define (any-expr data-map stripped-expr)
  (and (not (empty? data-map))
       (or (equal? stripped-expr (strip-boxes (caar data-map)))
           (any-expr (cdr data-map) stripped-expr))))
; Assumes that the specified element is not in the map. Adds it to the map. elem: recursively-boxed expression, outermost layer optional. Returns: recursively-boxed expression.
(define (add-element! boxdag expr) ; expr may be box, return expr, boxed if necessary
  (let ((boxed-expr
         (if (box? expr)
             expr
             (box expr))))
    (if (any-expr (get-data-map boxdag) (strip-boxes expr))
        (error "Entry is already in data map:" expr "::::" (get-data-map boxdag))
        (void))
    (add-entry! boxdag (cons (strip-outer-boxes boxed-expr) boxed-expr))
    boxed-expr))
; Get the pair for the specified recursively-boxed (outermost pair optional) expression. Result: (unboxed-element . boxed-element)
(define (get-boxdag-element-pair boxdag expr)
  (assoc (strip-outer-boxes expr) (get-data-map boxdag)))
; Get recursively-boxed expression for expr, with the top box removed if it would have to be created. See make-boxed!.
(define (get-boxed! boxdag expr)
  (cond ((box? expr)
         (if (box? (unbox expr))
             (get-boxed! boxdag (unbox expr))
             expr))
        ((pair? expr)
         (let ((found (get-boxdag-element-pair boxdag expr)))
           (if found
               (cdr found)
               (make-all-boxed! boxdag expr))))
        (else expr)))
; Get or make recursively-boxed expression for expr. expr: any expression, recursively-boxed or not or anywhere in-between.
(define (make-boxed! boxdag expr)
  (cond ((box? expr)
         (if (box? (unbox expr))
             (make-boxed! boxdag (unbox expr)) ; strip all layers but one
             expr))
        ((pair? expr)
         (let* ((subboxed (make-all-boxed! boxdag expr))
                (found (get-boxdag-element-pair boxdag subboxed)))
           (if found
               (cdr found)
               (add-element! boxdag subboxed))))
        (else expr)))
; Get or recursively make all every expression in exprs.
(define (make-all-boxed! boxdag exprs)
  (map (curry make-boxed! boxdag) exprs))
; Make a boxdag for expr.
(define (make-boxdag expr)
  (let ((boxdag (boxdag-struct empty empty empty)))
    (set-boxdag-struct-preserved! boxdag (list (cons null (make-boxed! boxdag expr))))
    boxdag))
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
; Strip any top-level double boxes from the boxdag
(define (optimize-boxdag boxdag)
  (set-boxdag-struct-data-map! boxdag (optimize (get-data-map boxdag) #f)))
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
