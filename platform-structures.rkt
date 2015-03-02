#lang racket

(provide platform-struct platform-struct-name platform-struct-registers platform-struct-instrs
         platform-struct-rules platform-struct-reg-remap-op platform-struct-label-framing
         platform-struct-function-framing platform-struct-pipeline platform-struct-boxdag-hooks
         platform-struct-simple-nodes)

(struct platform-struct
  (name registers instrs rules reg-remap-op label-framing function-framing pipeline boxdag-hooks simple-nodes) #:inspector #f)
