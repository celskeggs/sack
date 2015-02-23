#lang racket

(require "platform.rkt")
(require "platform-templates.rkt")

(provide tstk)

(platform tstk
          (stack-based)
          (use-standard-reductions)
          (argument-behavior argid (slot-get (const argid u4))
                             value (slot-set! (const argid u4) value))
          (localvar-behavior (varid argcount) (slot-get (+ (const varid u4) (const argcount u4)))
                             value (slot-set! (+ (const varid u4) (const argcount u4)) value))
          (label-framing-code (blockid get-export)
                              (" LABEL " blockid)
                              ())
          (function-framing-code (name touched get-export)
                                 ("FUNCTION " name)
                                 ("END FUNCTION"))
          (reduce-branch)
          (call-behavior-stack)
          (instructions
           [(tstk/slot? (id const?))
            ("  slot? " id)
            (push (slot-get id))]
           [(tstk/slot! (id const?) (source any?))
            ("  slot! " id)
            (raw slot-set! id (pop source))]
           [(tstk/const (value const?))
            ("  const " value)
            (push value)]
           [(tstk/add (a any?) (b any?))
            ("  add")
            (push (+ (pop a) (pop b)))]
           [(tstk/sub (a any?) (b any?))
            ("  sub")
            (push (- (pop a) (pop b)))]
           [(tstk/mul (a any?) (b any?))
            ("  mul")
            (push (* (pop a) (pop b)))]
           [(tstk/cmplt (a any?) (b any?))
            ("  cmplt")
            (push (< (pop a) (pop b)))]
           [(tstk/cmpgt (a any?) (b any?))
            ("  cmpgt")
            (push (> (pop a) (pop b)))]
           [(tstk/cmple (a any?) (b any?))
            ("  cmple")
            (push (<= (pop a) (pop b)))]
           [(tstk/cmpge (a any?) (b any?))
            ("  cmpge")
            (push (>= (pop a) (pop b)))]
           [(tstk/invoke (target symbol?))
            ("  invoke " target)
            (push (call-raw target))]
           [(tstk/jumpif (cond any?) (target number?))
            ("  jumpif ." target)
            (goto-if cond target)]
           [(tstk/jump (target number?))
            ("  jump ." target)
            (goto target)]
           [(tstk/ret (value any?))
            ("  ret")
            (return (pop value))]
          ))
