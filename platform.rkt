#lang racket

(provide platform reduction-raw reduction-simple reduction-simple-gen reduction-advanced reduction-calc const? any? instructions
         set-reg-remap-op! platform-apply platform-parse register? set-registers! label-framing-code function-framing-code
         platform-struct-pipeline platform-struct-registers run-platform-pipeline platform-pipeline-def boxdag-hook parser-simple-nodes)

(require racket/stxparam)
(require "utilities.rkt")
(require "common.rkt")
(require "boxdag.rkt")
(require "boxdag-rules.rkt")
(require "parser.rkt")
(require "boxdag-to-ssa.rkt")
(require "rule-generator.rkt")
(require "pipeline.rkt")
(require "platform-structures.rkt")
(require "stringify.rkt")

(struct mutable-platform-struct
  (name registers instrs rules reg-remap-op label-framing function-framing pipeline boxdag-hooks simple-nodes) #:mutable #:inspector #f)
(define (finalize-platform x)
  (platform-struct (mutable-platform-struct-name x)
                   (mutable-platform-struct-registers x)
                   (reverse (mutable-platform-struct-instrs x))
                   (reverse (mutable-platform-struct-rules x))
                   (mutable-platform-struct-reg-remap-op x)
                   (mutable-platform-struct-label-framing x)
                   (mutable-platform-struct-function-framing x)
                   (mutable-platform-struct-pipeline x)
                   (mutable-platform-struct-boxdag-hooks x)
                   (mutable-platform-struct-simple-nodes x)))

(define-syntax-rule (set-registers! regs)
  (set-mutable-platform-struct-registers! active-platform-ref regs))
(define-syntax-rule (set-reg-remap-op! op)
  (set-mutable-platform-struct-reg-remap-op! active-platform-ref op))
(define (add-platform-rule! platform-ref rule)
  (set-mutable-platform-struct-rules! platform-ref (cons rule (mutable-platform-struct-rules platform-ref))))
(define (add-platform-instr! platform-ref instr)
  (set-mutable-platform-struct-instrs! platform-ref (cons instr (mutable-platform-struct-instrs platform-ref))))
(define (add-platform-boxdag-hook! platform-ref hook)
  (set-mutable-platform-struct-boxdag-hooks! platform-ref (cons hook (mutable-platform-struct-boxdag-hooks platform-ref))))

(define-syntax-rule (label-framing-code (blockid exports) (start ...) (end ...))
  (set-mutable-platform-struct-label-framing! active-platform-ref
                                              (list (lambda (blockid exports) (list start ...))
                                                    (lambda (blockid exports) (list end ...)))))
(define-syntax-rule (function-framing-code (name touched exports) (start ...) (end ...))
  (set-mutable-platform-struct-function-framing! active-platform-ref
                                                 (list (lambda (name touched exports) (list start ...))
                                                       (lambda (name touched exports) (list end ...)))))

(define-syntax-parameter active-platform-ref
  (lambda (stx)
    (raise-syntax-error #f "use of active-platform-ref outside a platform declaration")))
(define-syntax-parameter register?
  (lambda (stx)
    (raise-syntax-error #f "use of register? outside a platform declaration")))
(define-syntax instruction
  (syntax-rules ()
    [(instruction (name (arg-name arg-type) ...) (string-part ...) instr-behavior)
     (make-instruction 'name
                       (list (list 'arg-name arg-type) ...)
                       (lambda (mapping)
                         (let ((arg-name (second (assoc 'arg-name mapping))) ...)
                           (list string-part ...)))
                       'instr-behavior
                       empty
                       empty)]
    [(instruction (name (arg-name arg-type) ...) (string-part ...) instr-behavior #:options (option ...))
     (make-instruction 'name
                       (list (list 'arg-name arg-type) ...)
                       (lambda (mapping)
                         (let ((arg-name (second (assoc 'arg-name mapping))) ...)
                           (list string-part ...)))
                       'instr-behavior
                       empty
                       '(option ...))]))
(define (is-trivial-rule rule) ; TODO: Use this to autoselect register movement operation.
  (and (symbol? (boxdag-rule-find rule))
       (= (length (boxdag-rule-args rule)) 1)
       (equal? (cdar (boxdag-rule-args rule)) any?)))
(define-syntax-rule (boxdag-hook hook)
  (add-platform-boxdag-hook! active-platform-ref hook))
(define-syntax-rule (parser-simple-nodes nodes)
  (set-mutable-platform-struct-simple-nodes! active-platform-ref nodes))
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
  (define name (let ((platform-def (mutable-platform-struct 'name (void) empty empty (void) (void) (void) empty-pipeline empty default-simple-nodes))
                     (is-register? (lambda (x) (error "No (register-based) declaration!"))))
                 (syntax-parameterize ([active-platform-ref (make-rename-transformer #'platform-def)]
                                       [register? (make-rename-transformer #'is-register?)])
                                      ; default pipeline entries
                                      (platform-pipeline-def (platform lisplike-source linear-source)
                                                             (parse lisplike-source #:simple-nodes (platform-struct-simple-nodes platform)))
                                      (platform-pipeline-def (platform linear-source source-header)
                                                             (list (first linear-source) (second linear-source) (third linear-source)))
                                      (platform-pipeline-def (platform linear-source ssa-assembly-with-exports)
                                                             (map (curry platform-process-block
                                                                         platform (first linear-source)
                                                                         #:exported `((argument-counts count ,(length (second linear-source)))
                                                                                      (internal-varcounts original ,(parse-varcount (cdddr linear-source)))))
                                                                  (cdddr linear-source)))
                                      (platform-pipeline-def (platform ssa-assembly-with-exports ssa-assembly)
                                                             (map first ssa-assembly-with-exports))
                                      (platform-pipeline-def (platform ssa-assembly-with-exports processed-exports)
                                                             (map second ssa-assembly-with-exports))
                                      (platform-pipeline-def (platform processed-exports condensed-exports)
                                                             (map-curry map condense-export-set processed-exports))
                                      (platform-pipeline-def (platform condensed-exports merged-exports)
                                                             (map condense-export-set (merge-export-sets condensed-exports empty)))
                                      (platform-pipeline-def (platform specified-assembly registers-touched source-header condensed-exports merged-exports textual-assembly)
                                                             (stringify platform (car source-header) specified-assembly registers-touched condensed-exports merged-exports))
                                      ; end default pipeline
                                      entry ...)
                 (finalize-platform platform-def))))

(define (platform-parse platform input)
  (let* ((parsed (parse input #:simple-nodes (platform-struct-simple-nodes platform)))
         (name (first parsed))
         (args (second parsed))
         (rettype (third parsed))
         (body (cdddr parsed)))
    (cons name (cons args (cons rettype (map (curry platform-process-block platform name) body))))))
(define (platform-process-block platform name block #:exported (exported empty))
  (assert (>= (length block) 1) "Each block must have at least one statement.")
  (let* (;(block-without-drops (map (lambda (x) (if (eq? (car x) 'drop) (cadr x) x)) block))
         (boxdag (platform-process platform name block;-without-drops
                                   #:exported exported))
         (boxdag-extracted (get-boxdag-contents boxdag))
         (boxdag-exported (get-boxdag-exports boxdag)))
    ; Note that full-ssaify will mangle the boxdag's contents, but that's fine here.
    (list (full-ssaify platform boxdag-extracted)
          boxdag-exported)))
(define pre-preservation-rules
  (list (boxdag-rule
         `((id . ,number?) (reg . ,symbol?) (base . ,any?) (ref . ,any?))
         '(boxdag/preserve-ref-prepared ref (generic/subresult id reg base))
         '(boxdag/preserve-ref-prepared-immediate ref (generic/subresult id reg (boxdag/preserve base))))))
(define (platform-apply platform boxdag)
  (apply-boxdag-rules-all (append pre-preservation-rules (platform-struct-rules platform)) boxdag
                          #:avoid-preserve '(generic/subresult call-raw)
                          #:hooks (map (lambda (x) (curry x platform)) (platform-struct-boxdag-hooks platform)))
  boxdag)
(define (platform-process platform name inputs #:exported (exported empty))
  (let-values (((most-inputs last-input) (split-at-right inputs 1)))
    (platform-apply platform (make-boxdag
                              (suffix (cons 'generic/middle-of
                                            (cons (list 'boxdag/export 'provided name name)
                                                  (map-curry list 'boxdag/preserve most-inputs)))
                                      (list 'generic/middle (cons 'boxdag/preserve last-input)))
                              #:exported exported))))
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
  (reduction-simple-gen (name (arg cond) ...) 'repl))
(define-syntax-rule (reduction-simple-gen (name (arg cond) ...) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   repl)))
(define-syntax-rule (reduction-calc (name (arg cond) ...) (cur-exports) repl)
  (add-platform-rule! active-platform-ref
                      (boxdag-rule (list (cons 'arg cond) ...)
                                   (list 'name (calc-fixup-arg 'arg cond) ...)
                                   (lambda (vars cur-exports)
                                     (let ((arg (cdr (assoc 'arg vars))) ...)
                                       repl)))))

(define (run-platform-pipeline platform code #:source (source 'lisplike-source) #:target (target 'textual-assembly) #:provide-result (provide-result #f))
  (define (reporter data-type data)
    (if (void? data)
        (displayln (string-append "=== " (~a data-type) " ==="))
        (if (string? data)
            (displayln data)
            (pretty-print data))))
  (let ((result
         (pipe-run-sched platform (platform-struct-pipeline platform)
                         code source target #:reporter reporter)))
    (when provide-result
      (if (eq? provide-result #t)
          result
          (second (assoc provide-result result))))))
