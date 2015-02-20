#lang racket

; Currently assuming register-based.

(provide make-instruction
         instruction-struct-name instruction-struct-arguments
         instruction-struct-returns instruction-struct-used-arguments
         instruction-struct-string-gen instruction-struct-behavior
         instruction-struct-rules instruction-struct-constraints)

(require "utilities.rkt")
(require "common.rkt")
(require "boxdag-rules.rkt")

(struct instruction-struct
  (name arguments returns used-arguments string-gen behavior rules constraints) #:inspector #f)

; arguments: ((name predicate) ...)
(define (convert-behavior-expr arguments constraints behavior)
  (if (symbol? behavior)
      (if (assoc behavior (unbox arguments))
          (let ((predicate (second (assoc behavior (unbox arguments)))))
            (cond [(eq? predicate const?) (list 'const behavior 'u4)] ; TODO: don't hardcode type
                  [(eq? predicate symbol?) behavior]
                  [else (error "Uncertain how to handle raw argument:" behavior)]))
          (error "Uncertain how to handle raw non-argument symbol:" behavior))
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
        (else
         (cons (car behavior) (map (curry convert-behavior-expr arguments constraints) (cdr behavior)))))))

(define (convert-behavior-line arguments returns constraints behavior)
  (case (car behavior)
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
     (if (null? conv-id)
         (cons name (map car arguments))
         (let ((reg-name (if (eq? (car indirect-behavior) 'set-reg) (second indirect-behavior) 'generic/unknown)))
           (list 'generic/subresult conv-id reg-name (cons name (map car arguments))))))))

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

(define (make-instruction name args string-gen behavior constraints)
  (let* ((mut-args (box args))
         (mut-constraints (box constraints))
         (mut-rets (box null))
         (behavior-out (convert-behavior name mut-args mut-rets behavior mut-constraints)))
    (instruction-struct name (unbox mut-args)
                        (reverse (unbox mut-rets)) (car behavior-out)
                        string-gen behavior (cdr behavior-out)
                        (unbox mut-constraints))))

;(convert-behavior 'x86/cmp/dd '((a any?) (b any?))
;                  '(multiple
;                    (set-reg carry-flag (unsigned< (get-reg a) (get-reg b)))
;                    (set-reg zero-flag (= (get-reg a) (get-reg b)))
;                    (set-reg sign-flag-xor-overflow-flag (< (get-reg a) (get-reg b)))))
