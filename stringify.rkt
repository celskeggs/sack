#lang racket

(require "platform.rkt")
(require "rule-generator.rkt")
(require "utilities.rkt")

(provide stringify)

(define (get-instruction platform name)
  (let ((matches (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs platform))))
    (assert (= (length matches) 1) (string-append "Expected exactly one instruction for name: " (symbol->string name)))
    (car matches)))

(define (stringify-elem elem)
  (cond ((string? elem) elem)
        ((symbol? elem) (symbol->string elem))
        ((integer? elem) (~a elem))))

(define (stringify-line plat line)
  (let* ((instruction (get-instruction plat (car line)))
        (format (instruction-struct-string-gen instruction))
        (arguments (cdr line)))
    (string-append* (map stringify-elem (format arguments)))))

(define (stringify-block plat seq)
  (append (list (string-append* (map stringify-elem
                                     ((first (platform-struct-label-framing plat)) (car seq)))))
          (map-curry stringify-line plat (cdr seq))
          (list (string-append* (map stringify-elem
                                     ((second (platform-struct-label-framing plat)) (car seq)))))))

(define (stringify plat name seqs-and-touched locals)
  (let ((seqs (first seqs-and-touched))
        (touched (second seqs-and-touched)))
    (string-join
     (append* (append
               (list (list (string-append* (map stringify-elem
                                                ((first (platform-struct-function-framing plat)) name locals touched)))))
               (map-curry stringify-block plat (enumerate seqs))
               (list (list (string-append* (map stringify-elem
                                                ((second (platform-struct-function-framing plat)) name locals touched)))))))
     "\n")))
