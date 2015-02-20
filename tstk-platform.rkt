#lang racket

(require "platform.rkt")
(require "platform-templates.rkt")

(provide tstk)

(platform tstk
          ;(stack-based)
          (use-standard-reductions)
          (argument-behavior argid (slot-get argid)
                             value (slot-set! argid value))
          (localvar-behavior (varid argcount) (slot-get (+ varid argcount))
                             value (slot-set! (+ varid argcount) value))
          (label-framing-code (blockid get-export)
                              ("LABEL " blockid)
                              ())
          (function-framing-code (name touched get-export)
                                 ("FUNCTION " name "\n")
                                 ("END FUNCTION\n"))
          (instructions
           [(tstk/slot? (dest any?) (id const?))
            ("slot? " id)
            (push dest (slot-get id))]
           [(tstk/slot! (id const?) (source any?))
            ("slot! " id)
            (slot-set! id (pop source))]
           [(tstk/const (value const?) (out any?))
            ("const" value)
            (push out value)]
           [(tstk/add (a any?) (b any?) (out any?))
            ("add")
            (push out (+ (pop b) (pop a)))]
           [(tstk/sub (a any?) (b any?) (out any?))
            ("sub")
            (push out (- (pop b) (pop a)))]
           [(tstk/invoke (target symbol?) (out any?))
            ("invoke " target)
            (push out (call target))]
           [(tstk/jumpif (cond any?) (target number?))
            ("jumpif ." target)
            (goto-if cond target)]
           [(tstk/jump (target number?))
            ("jump ." target)
            (goto target)]
           [(tstk/ret (value any?))
            ("ret")
            (return (pop value))]
          ))
