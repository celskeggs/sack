#lang racket

(provide platform reduction-raw reduction-simple reduction-advanced reduction-calc const? any? instructions set-reg-remap-op!
         platform-apply platform-process platform-parse platform-process-block register? set-registers! label-framing-code function-framing-code
         platform-struct-pipeline platform-struct-registers)

(require racket/stxparam)
(require "utilities.rkt")
(require "common.rkt")
(require "boxdag.rkt")
(require "boxdag-rules.rkt")
(require "parser.rkt")
(require "boxdag-to-ssa.rkt")
(require "rule-generator.rkt")
(require "register-constraints.rkt")
(require "pipeline.rkt")
(require "platform-structures.rkt")
(require "register-allocation.rkt")
(require "stringify.rkt")

(struct mutable-platform-struct
  (name registers instrs rules reg-remap-op label-framing function-framing pipeline) #:mutable #:inspector #f)
(define (finalize-platform x)
  (platform-struct (mutable-platform-struct-name x)
                   (mutable-platform-struct-registers x)
                   (reverse (mutable-platform-struct-instrs x))
                   (reverse (mutable-platform-struct-rules x))
                   (mutable-platform-struct-reg-remap-op x)
                   (mutable-platform-struct-label-framing x)
                   (mutable-platform-struct-function-framing x)
                   (mutable-platform-struct-pipeline x)))

(define-syntax-rule (set-registers! regs)
  (set-mutable-platform-struct-registers! active-platform-ref regs))
(define-syntax-rule (set-reg-remap-op! op)
  (set-mutable-platform-struct-reg-remap-op! active-platform-ref op))
(define (add-platform-rule! platform-ref rule)
  (set-mutable-platform-struct-rules! platform-ref (cons rule (mutable-platform-struct-rules platform-ref))))
(define (add-platform-instr! platform-ref instr)
  (set-mutable-platform-struct-instrs! platform-ref (cons instr (mutable-platform-struct-instrs platform-ref))))

(define-syntax-rule (label-framing-code (blockid) (start ...) (end ...))
  (set-mutable-platform-struct-label-framing! active-platform-ref
                                              (list (lambda (blockid) (list start ...))
                                                    (lambda (blockid) (list end ...)))))
(define-syntax-rule (function-framing-code (name locals touched) (start ...) (end ...))
  (set-mutable-platform-struct-function-framing! active-platform-ref
                                              (list (lambda (name locals touched) (list start ...))
                                                    (lambda (name locals touched) (list end ...)))))

(define-syntax-parameter active-platform-ref
  (lambda (stx)
    (raise-syntax-error #f "use of active-platform-ref outside a platform declaration")))
(define-syntax-parameter register?
  (lambda (stx)
    (raise-syntax-error #f "use of register? outside a platform declaration")))
(define-syntax-rule (instruction (name (arg-name arg-type) ...)
                                 (string-part ...)
                                 instr-behavior)
  (make-instruction 'name
                    (list (list 'arg-name arg-type) ...)
                    (lambda (mapping)
                      (let ((arg-name (second (assoc 'arg-name mapping))) ...)
                        (list string-part ...)))
                    'instr-behavior
                    empty))
(define (is-trivial-rule rule) ; TODO: Use this to autoselect register movement operation.
  (and (symbol? (boxdag-rule-find rule))
       (= (length (boxdag-rule-args rule)) 1)
       (equal? (cdar (boxdag-rule-args rule)) any?)))
(define-syntax-rule (instructions instr ...)
  (begin
    (begin
      (let ((i-struct (instruction . instr)))
        (add-platform-instr! active-platform-ref i-struct)
        (for ([rule (instruction-struct-rules i-struct)]
              #:unless (is-trivial-rule rule))
          (add-platform-rule! active-platform-ref rule)))) ...))
(define-syntax-rule (platform-pipeline-def code ...)
  (set-mutable-platform-struct-pipeline! active-platform-ref
                                         (pipe-def (mutable-platform-struct-pipeline active-platform-ref)
                                                   code ...)))
(define-syntax-rule (platform name entry ...)
  (define name (let ((platform-def (mutable-platform-struct 'name (void) empty empty (void) (void) (void) empty-pipeline))
                     (is-register? (lambda (x) (error "No (register-based) declaration!"))))
                 (syntax-parameterize ([active-platform-ref (make-rename-transformer #'platform-def)]
                                       [register? (make-rename-transformer #'is-register?)])
                                      ; default pipeline entries
                                      (platform-pipeline-def (platform lisplike-source linear-source)
                                                             (parse lisplike-source))
                                      (platform-pipeline-def (platform linear-source source-header)
                                                             (list (first linear-source) (second linear-source) (third linear-source)))
                                      (platform-pipeline-def (platform linear-source ssa-assembly)
                                                             (map-curry platform-process-block platform (cdddr linear-source)))
                                      (platform-pipeline-def (platform ssa-assembly register-constraints)
                                                             (register-constrain platform ssa-assembly))
                                      (platform-pipeline-def (platform ssa-assembly register-assembly)
                                                             (register-allocate platform (platform-struct-registers platform) ssa-assembly))
                                      (platform-pipeline-def (platform register-assembly source-header textual-assembly)
                                                             (stringify platform (car source-header) register-assembly 0))
                                      ; end default pipeline
                                      entry ...)
                 (finalize-platform platform-def))))

(define (platform-parse platform input)
  (let* ((parsed (parse input))
         (name (first parsed))
         (args (second parsed))
         (rettype (third parsed))
         (body (cdddr parsed)))
    (cons name (cons args (cons rettype (map (curry platform-process-block platform) body))))))
(define (platform-process-block platform block)
  (assert (= (length block) 1) "Can only process one statement per block. (TODO)")
  (let ((boxdag-extracted (get-boxdag-contents (platform-process platform (car block)))))
    ; Note that full-ssaify will mangle the boxdag's contents, but that's fine here.
    (full-ssaify (platform-struct-reg-remap-op platform) boxdag-extracted)))
(define pre-preservation-rules
  (list (boxdag-rule
         `((id . ,number?) (reg . ,symbol?) (base . ,any?) (ref . ,any?))
         '(boxdag/preserve-ref-prepared ref (generic/subresult id reg base))
         '(boxdag/preserve-ref-prepared-immediate ref (generic/subresult id reg (boxdag/preserve base))))))
(define (platform-apply platform boxdag)
  (apply-boxdag-rules-all (append pre-preservation-rules (platform-struct-rules platform)) boxdag #:avoid-preserve '(generic/subresult call-raw))
  boxdag)
(define (platform-process platform input)
  (platform-apply platform (make-boxdag input)))
(define (calc-fixup-arg arg cond)
  (if (eq? cond const?)
      (wrap-const arg) ; TODO: don't hard-code types
      arg))
(define (calc-fixup-recurse args data)
  (cond ((pair? data)
         (cons (calc-fixup-recurse args (car data))
               (calc-fixup-recurse args (cdr data))))
        ((and (assoc data args) (eq? (cdr (assoc data args)) const?))
         (wrap-const data))
        (else data)))
(define-syntax-rule (reduction-raw args find repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule args find repl)))
(define-syntax-rule (reduction-advanced (arg cond) ... find repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...) (calc-fixup-recurse (list (cons 'arg cond) ...) 'find) 'repl)))
(define-syntax-rule (reduction-simple (name (arg cond) ...) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   'repl)))
(define-syntax-rule (reduction-calc (name (arg cond) ...) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   (lambda (vars)
                                     (let ((arg (cdr (assoc 'arg vars))) ...)
                                       repl)))))
