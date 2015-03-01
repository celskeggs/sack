#lang racket

(provide make-instruction
         instruction-struct-name instruction-struct-arguments
         instruction-struct-returns instruction-struct-used-arguments
         instruction-struct-string-gen instruction-struct-behavior
         instruction-struct-rules instruction-struct-constraints
         instruction-struct-options instruction-option?)

(require "utilities.rkt")
(require "common.rkt")
(require "boxdag-rules.rkt")

(struct instruction-struct
  (name arguments returns used-arguments string-gen behavior rules constraints options) #:inspector #f)

(define (instruction-option? instruction option)
  (and (member option (instruction-struct-options instruction)) #t))

; arguments: ((name predicate) ...)
(define (convert-behavior-expr arguments constraints behavior)
  (cond ((symbol? behavior)
         (if (assoc behavior (unbox arguments))
             (let ((predicate (second (assoc behavior (unbox arguments)))))
               (cond [(eq? predicate const?) (list 'const behavior 'u4)] ; TODO: don't hardcode type
                     [(member predicate (list symbol? number? integer? list? string?)) behavior]
                     [else (error "Uncertain how to handle raw argument:" behavior)]))
             (error "Uncertain how to handle raw non-argument symbol:" behavior)))
        ((empty? behavior)
         empty)
        (else
         (case (car behavior)
           ('get-reg
            (assert (= (length behavior) 2) "get-reg expects one argument")
            (assert (symbol? (second behavior)) "get-reg expects a symbol argument")
            (if (assoc (second behavior) (unbox arguments))
                (second behavior)
                (begin
                  (set-box! arguments (cons (list (second behavior) any?) (unbox arguments)))
                  (set-box! constraints (cons (cons (second behavior) (second behavior)) (unbox constraints)))
                  (second behavior)
                  )))
           ('pop
            (assert (= (length behavior) 2) "pop expects one argument")
            (assert (symbol? (second behavior)) "pop expects a symbol argument")
            (let ((pop (assoc (second behavior) (unbox arguments))))
              (assert pop "pop expects a reference argument")
              (second behavior)))
           (else
            (cons (car behavior) (map-and-tail (curry convert-behavior-expr arguments constraints) (cdr behavior))))))))
  
(define (convert-behavior-line arguments returns constraints behavior)
  (case (car behavior)
    ('push
     (assert (= (length behavior) 2) "push expects one argument")
     (set-box! returns (cons 'pushed (unbox returns)))
     (convert-behavior-expr arguments constraints (second behavior)))
    ('raw
     (assert (symbol? (second behavior)) "raw expects one symbol")
     (cons (second behavior) (map (curry convert-behavior-expr arguments constraints) (cddr behavior))))
    ('set-reg
     (assert (= (length behavior) 3) "set-reg expects two arguments")
     (unless (assoc (second behavior) (unbox arguments))
       (set-box! arguments (cons (list (second behavior) any?) (unbox arguments)))
       (set-box! constraints (cons (cons (second behavior) (second behavior)) (unbox constraints))))
     (set-box! returns (cons (second behavior) (unbox returns)))
     (convert-behavior-expr arguments constraints (third behavior)))
    ('set-memory!
     (assert (= (length behavior) 3) "set-memory! expects two arguments")
     (list 'set-memory!
           (convert-behavior-expr arguments constraints (second behavior))
           (convert-behavior-expr arguments constraints (third behavior))))
    ('discard
     (assert (= (length behavior) 2) "discard expects one argument")
     (convert-behavior-expr arguments constraints (second behavior)))
    ('return
     (assert (= (length behavior) 2) "return expects one argument")
     (list 'return (convert-behavior-expr arguments constraints (second behavior))))
    ('goto
     (assert (= (length behavior) 2) "goto expects one argument")
     (list 'goto (second behavior)))
    ('goto-if
     (assert (= (length behavior) 3) "goto-if expects two arguments")
     (list 'goto-if (second behavior) (third behavior)))
    ('goto-if-not
     (assert (= (length behavior) 3) "goto-if-not expects two arguments")
     (list 'goto-if-not (second behavior) (third behavior)))
    (else (error "Unexpected behavior type" (car behavior)))))

(define (check-used-in zone arg-pair)
  (let ((arg (car arg-pair)))
    (define (used-in-iter part)
      (or (eq? part arg)
          (and (pair? part)
               (or (used-in-iter (car part))
                   (used-in-iter (cdr part))))))
    (used-in-iter zone)))

(define (build-converted-rule name arguments indirect-behavior enum-pair)
  (let ((conv-id (car enum-pair))
        (conv (cdr enum-pair)))
    (boxdag-rule
     (map (lambda (x)
            (assert (= (length x) 2) "Bad argument declaration")
            (cons (first x) (second x))) arguments)
     conv
     (let ((base
            (if (and (not (empty? arguments)) (eq? (second (last arguments)) list?))
                (cons name (apply list* (suffix (map car (drop-right arguments 1)) (car (last arguments)))))
                (cons name (map car arguments)))))
       (if (null? conv-id)
           base
           (let ((reg-name (if (eq? (car indirect-behavior) 'set-reg) (second indirect-behavior) 'generic/unknown)))
             (list 'generic/subresult conv-id reg-name base)))))))

(define (convert-behavior name arguments returns behavior constraints)
  (if (eq? (car behavior) 'multiple)
      (let* ((convs (map (curry convert-behavior-line arguments returns constraints) (cdr behavior)))
             (used-arguments (filter (curry check-used-in convs) (unbox arguments))))
        (cons used-arguments
              (map (curry build-converted-rule name used-arguments) (cdr behavior) (enumerate convs))))
      (let* ((conv (convert-behavior-line arguments returns constraints behavior))
             (used-arguments (filter (curry check-used-in conv) (unbox arguments))))
        (cons used-arguments
              (list (build-converted-rule name used-arguments #f (cons null conv)))))))

(define (make-instruction name args string-gen behavior constraints options)
  (let* ((mut-args (box args))
         (mut-constraints (box constraints))
         (mut-rets (box null))
         (behavior-out (convert-behavior name mut-args mut-rets behavior mut-constraints)))
    (instruction-struct name (unbox mut-args)
                        (reverse (unbox mut-rets)) (car behavior-out)
                        string-gen behavior (cdr behavior-out)
                        (unbox mut-constraints)
                        options)))
