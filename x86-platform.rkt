#lang racket

(require "utilities.rkt")

(define sample '(math (u4 u4) u4
                      ((swr 0 (arg 0))
                       (swr 1 (arg 1))
                       (swr 2 (+ (swr 0) (swr 1)))
                       (return (swr 2)))))

; x86 information
(define x86-platform '(platform x86
                                (registers eax ebx ecx edx esi edi)
                                (return-behavior returned-value
                                                 (set-register eax returned-value))
                                (argument-behavior argnum
                                                   (get-memory (+ (get-register ebp) (+ 8 (* argnum 4)))))
                                (instruction-delimiter "\n")
                                (definition-syntax "global " name "\n" name ":")
                                (jumptarget-syntax ".lab" name ":")
                                (instructions
                                 (inst (x86/movfm/c (register dest) (constant source))
                                       ("  mov " dest ", [" source "]")
                                       (behavior
                                        (set-reg dest (x86/get-memory/c (unconst source))))
                                       (yields
                                        (rule
                                         (source const?)
                                         (x86/get-memory/c source)
                                         (x86/movfm/c source))))
                                 (inst (x86/add/dc (register dest) (register source))
                                       ("  add " dest ", " source)
                                       (behavior
                                        (set-reg dest (+ (get-reg dest) (get-reg source))))
                                       (yields
                                        (rule
                                         (source any?) (dest any?)
                                         (+ dest source)
                                         (x86/add/dc dest source))))
                                 (inst (x86/mov/d (register dest) (register source))
                                       ("  mov " dest ", " source)
                                       (behavior
                                        (set-reg dest (get-reg source)))
                                       (yields
                                        (rule
                                         (source any?)
                                         source
                                         (x86/mov/d source)
                                        ))) ; would yield a nullary rule.
                                 (inst (x86/mov/c (register dest) (constant source))
                                       ("  mov " dest ", " source)
                                       (behavior
                                        (set-reg dest (unconst source)))
                                       (yields
                                        (rule
                                         (source const?)
                                         source
                                         (x86/mov/c (unconst source)))))
                                 )))

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
(define (add-boxdag-elem context elem) ; elem is a box, return elem
  (set-box! context (cons (cons (strip-outer-boxes elem) elem) (unbox context)))
  elem)
(define (make-boxdag-i context expr)
  ; context: #&((list . box-for-list) (list . box-for-list) ...)
  ; returns: boxed-expr
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (assoc (strip-outer-boxes expr) (unbox context))))
           (if found
               (cdr found)
               (add-boxdag-elem context (box (map (curry make-boxdag-i context) expr))))))
        (else expr)))
(define (make-boxdag expr)
  (let ((context (box empty)))
    (cons context (make-boxdag-i context expr))))
(define (make-boxdag-contents context expr)
  (cond ((box? expr) expr)
        ((pair? expr)
         (let ((found (assoc (strip-outer-boxes expr) (unbox context))))
           (if found
               (cdr found)
               (map (curry make-boxdag-i context) expr))))
        (else expr)))
(define (get-boxdag-map boxdag)
  (unbox (car boxdag)))
(define (get-boxdag-map-box boxdag)
  (car boxdag))
(define (get-boxdag-boxes boxdag)
  (map cdr (get-boxdag-map boxdag)))
(define (get-boxdag-content boxdag)
  (cdr boxdag))
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
(define (merge-match-results a b)
  (if (not (and a b))
      #f
      (merge-sorted-match-results a b)))
; argument args: ((arg . predicate-or-null) (arg . predicate-or-null) ...)
; returns: ((argname . value) (argname . value)) sorted by argname
(define (match-boxdag-rule args find element) ; TODO: check if the find is the same as the element, even with the boxes
  (cond ((box? element) (match-boxdag-rule args find (strip-outer-boxes element)))
        ((symbol? find)
         (let ((lookup (assoc find args)))
           (cond ((not lookup) (and (eq? find element) empty))
                 ((or (null? (cdr lookup))
                      ((cdr lookup) element))
                  (list (cons find element)))
                 (else #f))))
        ((xor (pair? find) (pair? element)) #f) ; either neither must be a pair or both must be
        ((not (pair? find)) (and (equal? find element) empty))
        ; both find and element are pairs
        (else 
         (let ((head (match-boxdag-rule args (car find) (car element))))
           (if head
               (merge-match-results head (match-boxdag-rule args (cdr find) (cdr element)))
               #f)))))
(define (apply-replacements vars in) ; vars: ((varname . value) (varname . value))
  (assert (not (box? in)) "Cannot have boxes in replacements!")
  (cond ((pair? in) (cons (apply-replacements vars (car in)) (apply-replacements vars (cdr in))))
        ((procedure? in) (in vars))
        (else (let ((found (assoc in vars)))
                (if found
                    (cdr found)
                    in)))))
(define (replace-boxdag-rule args find replace element)
  (let ((vars (match-boxdag-rule args find element)))
    (assert (equal? (sort (map car vars) symbol<?) (sort (map car args) symbol<?)) "Failed to match all arguments in boxdag rule.")
    (apply-replacements vars replace)))
(define (apply-boxdag-rule-once rule boxdag)
  (assert (= (length rule) 3) "Invalid rule.")
  (let ((args (first rule)) (find (second rule)) (replace (third rule)))
    (let ((found (findf
                  (curry match-boxdag-rule args find)
                  (map cdr (unbox boxdag)))))
      (if found
          (let ((calculated (make-boxdag-contents boxdag (replace-boxdag-rule args find replace (unbox found)))))
            (assert (not (equal? (strip-boxes calculated) (strip-boxes found))) "Expected replacer to modify the element!")
            (set-box! found calculated)
            (let ((lookup (assoc (strip-outer-boxes calculated) (unbox boxdag))))
              (if lookup
                  (set-box! found (cdr lookup))
                  (add-boxdag-elem boxdag found)))
            #t)
          #f))))
(define (apply-boxdag-rules-once rules boxdag)
  (and (not (empty? rules))
       (or (apply-boxdag-rule-once (car rules) boxdag)
           (apply-boxdag-rules-once (cdr rules) boxdag))))
(define (apply-boxdag-rules-all rules boxdag)
  (if (apply-boxdag-rules-once rules boxdag)
      (begin
        (apply-boxdag-rules-all rules boxdag)
        #t)
      #f))

(define-syntax-rule (boxdag-rule (arg cond) ... find repl)
  (list (list (cons 'arg cond) ...) 'find 'repl))
(define-syntax boxdag-rule-simple
  (syntax-rules ()
    [(boxdag-rule-simple (name (arg cond) ...) repl) (list (list (cons 'arg cond) ...) (list 'name 'arg ...) 'repl)]))
(define-syntax boxdag-rule-calc
  (syntax-rules ()
    [(boxdag-rule-simple (name (arg cond) ...) repl)
     (list (list (cons 'arg cond) ...) (list 'name 'arg ...)
           (lambda (vars)
             (let ((arg (cdr (assoc 'arg vars))) ...)
               repl)))]))
(begin-for-syntax
  (define (add-syntax-suffix stx name suffix)
    (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum name)) (symbol->string (syntax->datum suffix)))))))
(define-syntax boxdag-rule-variant
  (syntax-rules ()
    [(boxdag-rule-variant name (suffix-symbol (argument ...) (run-arg ...)) ...)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           [(name in out)
            #`(list
               (boxdag-rule-simple (in argument ...) (#,(add-syntax-suffix stx #'out #'suffix-symbol) run-arg ...)) ...)])))]))
(define (any? x) #t)
(define (const x) (list 'const x))
(define (const? x) (and (list? x) (= (length x) 2) (eq? (first x) 'const) (integer? (second x))))
(define (unconst x)
  (assert (const? x) "Expected a constant.")
  (second x))
(define (reg? x) (and (list? x) (= (length x) 2) (eq? (first x) 'reg)))
(define (mem? x) (and (list? x) (or (eq? (car x) 'get-memory)
                                    (eq? (car x) 'x86/get-memory/dc)
                                    (eq? (car x) 'x86/get-memory/dd))))

(define bd (make-boxdag '(+ (arg 0) (const 10))))
(boxdag-rule-variant x86/2op-comu
                     (/dc ((a any?) (b const?)) (a (unconst b)))
                     (/dc ((a const?) (b any?)) ((unconst b) a))
                     (/dm ((a any?) (b mem?)) (a b))
                     (/dm ((a mem?) (b any?)) (b a))
                     (/dd ((a any?) (b any?)) (a b)))
(boxdag-rule-variant x86/2op-ncom
                     (/dc ((a any?) (b const?)) (a b))
                     (/dm ((a any?) (b mem?)) (a b))
                     (/dd ((a any?) (b any?)) (a b)))
(define bd-rules
  (append
   (list
    (boxdag-rule-simple (arg-ref (argnum any?)) (get-memory (+ (get-base-ptr) (+ (const 8) (* argnum (const 4))))))
    (boxdag-rule-simple (arg (argnum number?)) (arg-ref (const argnum)))
    (boxdag-rule-calc (+ (a const?) (b const?)) (const (+ (unconst a) (unconst b))))
    (boxdag-rule-calc (* (a const?) (b const?)) (const (* (unconst a) (unconst b))))
    (boxdag-rule (a any?) (b const?) (get-memory (+ a b)) (x86/get-memory/dc a (unconst b)))
    (boxdag-rule (a const?) (b any?) (get-memory (+ a b)) (x86/get-memory/dc b (unconst a)))
    (boxdag-rule (a any?) (b any?) (get-memory (+ a b)) (x86/get-memory/dd a b))
    (boxdag-rule-calc (unconst (x const?)) (unconst x)))
   ; WORKING ON MEMORY REFERENCES...
   ; instr/dd, instr/dc, instr/cm, instr/cc, instr/mc, etc. d: dynamic, c: constant, m: memory
   (x86/2op-comu + x86/add)
   (x86/2op-ncom - x86/sub)
   (x86/2op-comu and x86/and)
   (x86/2op-comu or x86/or)
   (list
    (boxdag-rule-simple (get-base-ptr) (reg ebp)))
   ))
;bd
(strip-boxes (get-boxdag-content bd))
(apply-boxdag-rules-all bd-rules (get-boxdag-map-box bd))
;bd
(strip-boxes (get-boxdag-content bd))

(define-syntax-rule (instruction (name (arg type) ...)
                                 output-info
                                 behavior)
  (list 'name (cons 'arg type) ...
        'output-info
        (boxdag-rule (arg type) ...
                     stripped-behavior
                     (name arg ...))))
(define temp (instruction (x86/add/dc (dest any?) (source const?))
                           ("  add " dest ", " source)
                           (set-reg dest (+ (get-reg dest) (unconst source)))))
;                           (yields
;                            (rule
;                             (source any?) (dest any?)
;                             (+ dest source)
;                             (x86/add/dc dest source)))
