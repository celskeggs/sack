#lang racket

; Original version written in an hour and 25 minutes.

(require "platform.rkt")
(require "platform-templates.rkt")

(provide jvm jvm-link)

; Generated code should be compiled by Jasmin. Tested with v2.4.

(define (jvm-link classname sources #:include-main (include-main #f))
  (string-append ".class public " classname "\n"
                 ".super java/lang/Object\n"
                 (if include-main
                     (string-append ".method public static main([Ljava/lang/String;)V\n"
                                    ".limit stack 1\n"
                                    ".limit locals 1\n"
                                    "  invokestatic " classname "/main()I\n"
                                    "  invokestatic java/lang/System/exit(I)V\n"
                                    "  return\n"
                                    ".end method\n"
                                    ".method public static printf(Ljava/lang/String;II)I\n"
                                    ".limit stack 9\n"
                                    ".limit locals 3\n"
                                    "  getstatic java/lang/System/out Ljava/io/PrintStream;\n"
                                    "  aload 0\n"
                                    "  bipush 2\n"
                                    "  anewarray java/lang/Object\n"
                                    "  dup\n"
                                    "  bipush 0\n"
                                    "  new java/lang/Integer\n"
                                    "  dup\n"
                                    "  iload 1\n"
                                    "  invokespecial java/lang/Integer/<init>(I)V\n"
                                    "  aastore\n"
                                    "  dup\n"
                                    "  bipush 1\n"
                                    "  new java/lang/Integer\n"
                                    "  dup\n"
                                    "  iload 2\n"
                                    "  invokespecial java/lang/Integer/<init>(I)V\n"
                                    "  aastore\n"
                                    "  invokevirtual java/io/PrintStream/format(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;\n"
                                    "  pop\n"
                                    "  bipush 2\n"
                                    "  ireturn\n"
                                    ".end method\n")
                     "")
                 (string-replace (string-join sources "\n")
                                 "%CLASSNAME%"
                                 classname)))
(define (jvm-compile code)
  (run-platform-pipeline jvm code))

(define (string-repeat str count)
  (string-append* (map (const str) (range count))))
(define (make-argument-descriptor count)
  (string-append "(" (string-repeat "I" count) ")I"))

(define (escape-char c)
  (if (char=? c #\newline) "\\n"
      (string c)))
(define (escape-string str)
  (string-append* (map escape-char (string->list str))))

(platform jvm
          (stack-based)
          (use-standard-reductions)
          (argument-behavior argid (variable-get (const argid u4))
                             value (variable-set! (const argid u4) value))
          (localvar-behavior (varid argument-count) (variable-get (+ (const varid u4) (const argument-count u4)))
                             value (variable-set! (+ (const varid u4) (const argument-count u4)) value))
          (label-framing-code (blockid get-export)
                              ("L" blockid ":")
                              ())
          (function-framing-code (name touched get-export)
                                 (".method public static "
                                  name (make-argument-descriptor (get-num-used-arguments get-export)) "\n"
                                  ".limit stack " touched "\n"
                                  ".limit locals " (+ (get-num-used-arguments get-export) (get-num-used-locals get-export)))
                                 (".end method"))
          (parser-simple-nodes '(<= >= < > == != + - * / jvm/extcall))
          (reduce-branch)
          (call-behavior-stack)
          (booleans-as-0-1)
          ; based on call-behavior-stack
          (reduction-raw (list (cons 'target string?) (cons 'args list?))
                   '(jvm/extcall (const-string target) . args)
                   (lambda (vars cur-exports)
                     (let ((target (cdr (assoc 'target vars)))
                           (args (cdr (assoc 'args vars))))
                       `(boxdag/preserve (jvm/extcall-n ,target . ,args)))))
          (instructions
           [(jvm/ireturn (value any?))
            ("  ireturn")
            (return (pop value))]
           [(jvm/iload (ref const?))
            ("  iload " ref)
            (push (variable-get ref))]
           [(jvm/istore (ref const?) (value any?))
            ("  istore " ref)
            (discard (drop (variable-set! ref (pop value))))]
           [(jvm/iadd (a any?) (b any?))
            ("  iadd")
            (push (+ (pop a) (pop b)))]
           [(jvm/isub (a any?) (b any?))
            ("  isub")
            (push (- (pop a) (pop b)))]
           [(jvm/ldc/int (value const?))
            ("  ldc " value)
            (push value)]
           [(jvm/ldc/string (value string?))
            ("  ldc \"" (escape-string value) "\"")
            (push (const-string value))]
           [(jvm/if_icmpeq (target number?) (a any?) (b any?))
            ("  if_icmpeq L" target)
            (goto-if (== (pop a) (pop b)) target)]
           [(jvm/if_icmpne (target number?) (a any?) (b any?))
            ("  if_icmpne L" target)
            (goto-if (!= (pop a) (pop b)) target)]
           [(jvm/if_icmplt (target number?) (a any?) (b any?))
            ("  if_icmplt L" target)
            (goto-if (< (pop a) (pop b)) target)]
           [(jvm/if_icmpgt (target number?) (a any?) (b any?))
            ("  if_icmpgt L" target)
            (goto-if (> (pop a) (pop b)) target)]
           [(jvm/if_icmple (target number?) (a any?) (b any?))
            ("  if_icmple L" target)
            (goto-if (<= (pop a) (pop b)) target)]
           [(jvm/if_icmpge (target number?) (a any?) (b any?))
            ("  if_icmpge L" target)
            (goto-if (>= (pop a) (pop b)) target)]
           [(jvm/goto (target number?))
            ("  goto L" target)
            (goto target)]
           [(jvm/invokestatic/external (name+desc string?) (args list?))
            ("  invokestatic %CLASSNAME%/" name+desc)
            (push (jvm/extcall-n name+desc . args))]
           [(jvm/invokestatic/local (name symbol?) (args list?))
            ("  invokestatic %CLASSNAME%/" name (make-argument-descriptor (length args)))
            (push (call-raw-n name . args))]
           [(jvm/pop (value any?))
            ("  pop")
            (discard (drop (pop value)))]
           ))
