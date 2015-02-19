#lang racket

(require "platform-structures.rkt")
(require "rule-generator.rkt")
(require "utilities.rkt")
(require "common.rkt")

(provide stringify)

(define (get-instruction platform name)
  (let ((matches (filter (lambda (found) (eq? name (instruction-struct-name found))) (platform-struct-instrs platform))))
    (assert (= (length matches) 1) (string-append "Expected exactly one instruction for name: " (symbol->string name)))
    (car matches)))

(define (stringify-elem elem)
  (cond ((string? elem) elem)
        ((symbol? elem) (symbol->string elem))
        ((integer? elem) (~a elem))
        ((const-ref? elem) (symbol->string (const-ref-refname elem))))) ; TODO: manual control over stringifying of const-refs.

(define (stringify-line plat line)
  (let* ((instruction (get-instruction plat (car line)))
        (format (instruction-struct-string-gen instruction))
        (arguments (cdr line)))
    (string-append* (map stringify-elem (format arguments)))))

(define (stringify-block plat seq)
  (let* ((id (first seq))
         (code (second seq))
         (exports (third seq))
         (exportf (lambda (name) (second (or (assoc name exports) (list empty empty))))))
    (append (list (string-append* (map stringify-elem
                                       ((first (platform-struct-label-framing plat)) id exportf))))
            (map-curry stringify-line plat code)
            (list (string-append* (map stringify-elem
                                       ((second (platform-struct-label-framing plat)) id exportf)))))))

(define (stringify plat name seqs touched locals uncondensed-exported)
  (let* ((exported (map-curry map condense-export-set uncondensed-exported))
         (merged-exports (merge-export-sets exported empty))
         (merged-exportf (lambda (name) (second (or (assoc name merged-exports) (list empty empty))))))
    (string-join
     (append* (append
               (list (list (string-append* (map stringify-elem
                                                ((first (platform-struct-function-framing plat)) name locals touched merged-exportf)))))
               (map-curry stringify-block plat (map suffix (enumerate seqs #:combiner list) exported))
               (list (list (string-append* (map stringify-elem
                                                ((second (platform-struct-function-framing plat)) name locals touched merged-exportf)))))))
     "\n")))

(define (condense-export-set set)
  (list (car set)
        (unique #:cmp< (curry pair<? #:cmp< symbol<?) (cadr set))))

(define (merge-two-export-sets set-a set-b)
  (let ((all-names (unique (map car (append set-a set-b)) #:cmp< symbol<?)))
    (for/list ((name all-names))
      (let* ((found-a (assoc name set-a))
             (found-b (assoc name set-b)))
        (list name
              (append (if found-a (second found-a) empty)
                      (if found-b (second found-b) empty)))))))

(define (merge-export-sets sets populated)
  (if (empty? sets) populated
      (merge-export-sets (cdr sets) (merge-two-export-sets (car sets) populated))))

(define (pair<? a b #:cmp< (cmp< <) #:cmp= (cmp= equal?))
  (or (cmp< (first a) (first b))
      (and (cmp= (first a) (first b))
           (cmp< (second a) (second b)))))
