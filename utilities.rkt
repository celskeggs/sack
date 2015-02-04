#lang racket

(provide zip unzip without assert tree->string trace find-index list->stream stream-append-stream enumerate suffix unique map-curry without-presorted and* map->list comment map-join)

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
  (pretty-print args)
  (newline))

(define-syntax-rule (assert x failmsg ...)
  (if x
      x
      (error (string-join (list (~a failmsg) ...) " "))))

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
        ((equal? (first seq) (second seq)) (unique-i (cdr seq)))
        (else (cons (first seq) (unique-i (cdr seq))))))

(define (unique x #:cmp< [comparer symbol<?])
  (unique-i (sort x comparer)))

(define-syntax-rule (map-curry func args ... list)
  (map (curry func args ...) list))

(define (without-presorted coll without #:cmp< comparer)
  (cond ((empty? coll) empty)
        ((empty? without) coll)
        ((comparer (car coll) (car without)) (cons (car coll) (without-presorted (cdr coll) without #:cmp< comparer)))
        ((comparer (car without) (car coll)) (without-presorted coll (cdr without) #:cmp< comparer))
        (else (without-presorted (cdr coll) without #:cmp< comparer))))

(define (and* list)
  (or (empty? list) (and (car list) (and* (cdr list)))))

(define (map->list mapping)
  (let ((keys (unique (map first mapping) #:cmp< <)))
    (assert (= (length keys) (length mapping)) "Expected no duplicate keys!")
    (assert (and* (map integer? keys)) "Keys must be integers!")
    (assert (= (apply min keys) 0) "Keys must start at zero!")
    (assert (= (apply max keys) (- (length keys) 1)) "Keys must end at length-1!")
    (map (lambda (i) (second (assoc i mapping))) (range 0 (length keys)))))

(define-syntax-rule (comment arg ...)
  (void))

(define (map-join a b)
  (cond ((empty? a) b)
        ((assoc (caar a) b)
         (assert (equal? (cadar a) (second (assoc (caar a) b))) "map-join got conflicting values!")
         (map-join (cdr a) b))
        (else (cons (car a) (map-join (cdr a) b)))))
