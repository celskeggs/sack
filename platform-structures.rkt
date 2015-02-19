#lang racket

(provide platform-struct platform-struct-name platform-struct-registers platform-struct-instrs
         platform-struct-rules platform-struct-reg-remap-op platform-struct-label-framing
         platform-struct-function-framing platform-struct-pipeline platform-struct-export-processors)

(struct platform-struct
  (name registers instrs rules reg-remap-op label-framing function-framing export-processors pipeline) #:inspector #f)
