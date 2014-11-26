#lang racket

(require "utilities.rkt")

(provide parse)

(define sample '(fib ((n u4)) u4
                     (if (<= n 1)
                         1
                         (+ (fib (- n 1))
                            (fib (- n 2))))))

(define (parse-arg args)
  ; Note that enumerate adds a prefix to the arguments
  (assert (and (= (length args) 3) (number? (car args)) (symbol? (cadr args)) (symbol? (caddr args))) "Expected two symbols per argument.")
  (list (cadr args) (caddr args) 'arg (car args)))
(define (var-name v)
  (car v))
(define (var-type v)
  (cadr v))
(define (var-expr v)
  (cddr v))

(define simple-nodes '(<= >= < > == != + - * / %))

(define (parse-expr-noded type args vars)
  (cond ((member type simple-nodes)
         (cons type (map (lambda (expr) (parse-expr expr vars)) args)))
        (else
         (cons 'call (cons type (map (lambda (expr) (parse-expr expr vars)) args))))))

(define (parse-expr tree vars)
  (cond ((pair? tree) (parse-expr-noded (car tree) (cdr tree) vars))
        ((number? tree) `(const ,tree u4))
        ((assoc tree vars) (var-expr (assoc tree vars)))
        (else (error "Cannot parse expression" tree))))

(define (parse-stmt tree vars retexpr)
  (cond ((and (pair? tree) (eq? (first tree) 'if))
         (assert (= (length tree) 4) "If requires three arguments.")
         (list (list 'branch
                     (parse-expr (second tree) vars)
                     (make-placeholder (parse-stmt (third tree) vars retexpr))
                     (make-placeholder (parse-stmt (fourth tree) vars retexpr)))))
        ((and (pair? tree) (pair? (car tree)) (empty? (cdr tree)))
         (parse-stmt (car tree) vars retexpr))
        ((and (pair? tree) (pair? (car tree)))
         (let* ((goto-target (make-placeholder 'unset))
                (n-retexpr (lambda (retval) '((drop ,retval) (goto ,goto-target)))))
           (placeholder-set! goto-target (parse-stmt (cdr tree) vars retexpr))
           (parse-stmt (car tree) vars n-retexpr)))
        (else
         (retexpr (parse-expr tree vars)))))

(define (blockify-and-insert output-sequence tree placeholder)
  (if (number? tree)
      output-sequence
      (begin
        (placeholder-set! placeholder (length output-sequence))
        (blockify output-sequence tree))))

(define (blockify-traverse output-sequence tree)
  (cond ((pair? tree)
         (blockify-traverse (blockify-traverse output-sequence (car tree)) (cdr tree)))
        ((placeholder? tree)
         (blockify-and-insert output-sequence (placeholder-get tree) tree))
        (else output-sequence)))

(define (blockify output-sequence tree)
  (blockify-traverse (suffix output-sequence tree) tree))

(define (parse tree)
  (let ((name (car tree)) (args (map parse-arg (enumerate (cadr tree)))) (rettype (caddr tree)) (body (cdddr tree)))
    (cons name
          (cons (map var-type args)
                (cons rettype
                      (make-reader-graph (blockify '() (parse-stmt body args (lambda (retval) (list (list 'return retval)))))))))))

;(parse sample)
