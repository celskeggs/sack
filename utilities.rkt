#lang racket

(provide zip unzip without assert tree->string trace find-index list->stream stream-append-stream enumerate suffix unique symbol<?)

(define (zip a b)
  (cond ((and (empty? a) (empty? b)) empty)
        ((or (empty? a) (empty? b)) (error "Length mismatch to zip!"))
        (else (cons (list (car a) (car b)) (zip (cdr a) (cdr b))))))

(define (unzip x)
  (apply map (cons list x)))

(define (tree->string inst)
  (define o (open-output-string))
  (display inst o)
  (get-output-string o))

(define (without from remove)
  (filter (lambda (x) (not (member x remove))) from))

(define (suffix seq end)
  (append seq (list end)))

(define (trace . args)
  (print args)
  (newline))

(define (assert x failmsg)
  (if x
      x
      (error failmsg)))

(define (find-index-i predicate sequence i)
  (cond ((empty? sequence) (error "No match in list!"))
        ((predicate (car sequence)) i)
        (else (find-index-i predicate (cdr sequence) (+ i 1)))))

(define (find-index predicate sequence)
  (find-index-i predicate sequence 0))

(define (list->stream list)
  (if (empty? list) empty-stream
      (stream-cons (car list) (list->stream (cdr list)))))

(define (stream-append-stream stream-stream)
  (define (s-a-s-i active more)
    (if (stream-empty? active) (stream-append-stream more)
        (stream-cons (stream-first active) (s-a-s-i (stream-rest active) more))))
  (if (stream-empty? stream-stream)
      empty-stream
      (s-a-s-i (stream-first stream-stream) (stream-rest stream-stream))))

(define (enumerate x)
  (define (enumerate-i x i)
    (if (empty? x)
        empty
        (cons (cons i (car x)) (enumerate-i (cdr x) (+ i 1)))))
  (enumerate-i x 0))

(define (unique-i seq)
  (cond ((empty? seq) empty)
        ((empty? (cdr seq)) seq)
        ((eq? (first seq) (second seq)) (unique-i (cdr seq)))
        (else (cons (first seq) (unique-i (cdr seq))))))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (unique x)
  (unique-i (sort x symbol<?)))
