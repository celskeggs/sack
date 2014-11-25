#lang racket

(provide strip-boxes strip-outer-boxes get-data-boxes get-boxed! add-element! make-boxdag get-boxdag-content get-boxdag-element-pair)

(struct boxdag-struct
  (data-map  ; ((unboxed . recursively-boxed) (unboxed . recursively-boxed) ...)
   contents) ; recursively-boxed primary expression
  #:mutable)

(define (get-data-map boxdag)
  (boxdag-struct-data-map boxdag))
(define (set-data-map! boxdag updated)
  (set-boxdag-struct-data-map! boxdag updated))
(define (get-data-boxes boxdag)
  (map cdr (get-data-map boxdag)))
(define (get-boxdag-content boxdag)
  (boxdag-struct-contents boxdag))

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
; Assumes that the specified element is not in the map. Adds it to the map. elem: recursively-boxed expression, outermost layer optional. Returns: recursively-boxed expression.
(define (add-element! boxdag expr) ; expr may be box, return expr, boxed if necessary
  (let ((boxed-expr
         (if (box? expr)
             expr
             (box expr))))
    (add-entry! boxdag (cons (strip-outer-boxes boxed-expr) boxed-expr))
    boxed-expr))
; Get the pair for the specified recursively-boxed (outermost pair optional) expression. Result: (unboxed-element . boxed-element)
(define (get-boxdag-element-pair boxdag expr)
  (assoc (strip-outer-boxes expr) (get-data-map boxdag)))
; Get recursively-boxed expression for expr, with the top box removed if it would have to be created. See make-boxed!.
(define (get-boxed! boxdag expr)
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (get-boxdag-element-pair boxdag expr)))
           (if found
               (cdr found)
               (make-all-boxed! boxdag expr))))
        (else expr)))
; Get or make recursively-boxed expression for expr. expr: any expression, recursively-boxed or not or anywhere in-between.
(define (make-boxed! boxdag expr)
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (get-boxdag-element-pair boxdag expr)))
           (if found
               (cdr found)
               (add-element! boxdag (make-all-boxed! boxdag expr)))))
        (else expr)))
; Get or recursively make all every expression in exprs.
(define (make-all-boxed! boxdag exprs)
  (map (curry make-boxed! boxdag) exprs))
; Make a boxdag for expr.
(define (make-boxdag expr)
  (let ((boxdag (boxdag-struct empty (void))))
    (set-boxdag-struct-contents! boxdag (make-boxed! boxdag expr))
    boxdag))
