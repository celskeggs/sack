#lang racket

(require "utilities.rkt")

(provide parse)

(define (parse-arg args)
  (trace 'parse-arg args) ;(0 n u4)
  ; Note that enumerate adds a prefix to the arguments
  (assert (and (= (length args) 3) (number? (car args)) (symbol? (cadr args)) (symbol? (caddr args))) "Expected two symbols per argument.")
  (list (cadr args) (caddr args) 'arg (car args)))
(define (var-name v)
  (car v))
(define (var-type v)
  (cadr v))
(define (var-expr v)
  (cddr v))
(define (add-var v t args)
  (cons (list v t 'local (+ 1 (apply max (cons -1 ; WARNING: this may cause overlap???
                                               (filter-map (lambda (elem) (and (eq? (third elem) 'local)
                                                                               (fourth elem)))
                                                           args)))))
        args))

(define simple-nodes '(<= >= < > == != + - * / %))

(define (parse-expr-noded type args vars)
  (cond ((member type simple-nodes)
         (cons type (map (lambda (expr) (parse-expr expr vars)) args)))
        ((eq? type 'set!)
         (assert (= (length args) 2) "set! requires two arguments.")
         (let ((lookup (assoc (first args) vars)))
           (assert lookup (string-append "set! cannot find variable: " (symbol->string (first args))))
           (list (string->symbol (string-append "set-" (symbol->string (caddr lookup)) "!")) (cadddr lookup) (parse-expr (second args) vars))))
        (else
         (cons 'call (cons type (map (lambda (expr) (parse-expr expr vars)) args))))))

(define (parse-expr tree vars)
  (cond ((pair? tree) (parse-expr-noded (car tree) (cdr tree) vars))
        ((number? tree) `(const ,tree u4))
        ((string? tree) `(const-string ,tree))
        ((symbol? tree) (if (assoc tree vars) (var-expr (assoc tree vars)) (error "No such variable:" tree)))
        (else (error "Cannot parse expression" tree))))

(define (is-jumper tree)
  (and (pair? tree) (member (car tree) '(if while))))

(define (parse-stmt-list tree vars retexpr)
  (cond ((and (pair? tree) (pair? (car tree)) (eq? (caar tree) 'def))
         (assert (= (length (car tree)) 3) "def requires two arguments.")
         (parse-stmt-list (cdr tree) (add-var (cadar tree) (caddar tree) vars) retexpr))
        ((and (pair? tree) (pair? (car tree)) (empty? (cdr tree)))
         (parse-stmt (car tree) vars retexpr))
        ((and (pair? tree) (pair? (car tree)) (is-jumper (car tree)))
         (let* ((goto-target (make-placeholder (parse-stmt-list (cdr tree) vars retexpr))))
           (parse-stmt (car tree) vars (lambda (retval) `((drop ,retval) (goto ,goto-target))))))
        ((and (pair? tree) (pair? (car tree)))
         (append (parse-stmt (car tree) vars (lambda (retval) `((drop ,retval))))
                 (parse-stmt-list (cdr tree) vars retexpr)))
        ((and (pair? tree) (empty? (cdr tree)))
         (parse-stmt (car tree) vars retexpr))))

(define (parse-stmt tree vars retexpr)
  (cond ((and (pair? tree) (eq? (first tree) 'if))
         (assert (= (length tree) 4) "if requires three arguments.")
         (list (list 'branch
                     (parse-expr (second tree) vars)
                     (make-placeholder (parse-stmt (third tree) vars retexpr))
                     (make-placeholder (parse-stmt (fourth tree) vars retexpr)))))
        ((and (pair? tree) (eq? (first tree) 'while))
         (assert (>= (length tree) 1) "while requires at least one argument.")
         (let ((condition (make-placeholder (void)))
               (body (make-placeholder (void))))
           (placeholder-set! condition (list (list 'branch
                                                   (parse-expr (second tree) vars)
                                                   body
                                                   (let ((return-statement (filter-useless (retexpr '(parser/undefined)))))
                                                     (if (and (= (length return-statement) 1) (= (length (car return-statement)) 2) (eq? (caar return-statement) 'goto))
                                                         (cadar return-statement) ; go directly instead of gotoing to a goto block
                                                         (make-placeholder return-statement))))))
           (placeholder-set! body (parse-stmt-list (cddr tree) vars (lambda (retval) `((drop ,retval) (goto ,condition)))))
           (list (list 'goto condition))))
        (else
         (retexpr (parse-expr tree vars)))))

(define (filter-useless statements)
  (if (number? statements) statements
      (filter-not (curry equal? '(drop (parser/undefined))) statements)))

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
         (blockify-and-insert output-sequence (filter-useless (placeholder-get tree)) tree))
        (else output-sequence)))

(define (blockify output-sequence tree)
  (blockify-traverse (suffix output-sequence tree) tree))

(define (parse tree)
  (let ((name (car tree)) (args (map parse-arg (enumerate (cadr tree)))) (rettype (caddr tree)) (body (cdddr tree)))
    (cons name
          (cons (map var-type args)
                (cons rettype
                      (make-reader-graph
                       (blockify '() (filter-useless
                                      (parse-stmt-list body args
                                                       (lambda (retval) (list (list 'return retval))))))))))))
