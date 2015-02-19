#lang racket

(require "platform-structures.rkt")
(require "rule-generator.rkt")
(require "utilities.rkt")
(require "common.rkt")

(provide stringify stringify-exports)

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
  (append (list (string-append* (map stringify-elem
                                     ((first (platform-struct-label-framing plat)) (car seq)))))
          (map-curry stringify-line plat (cdr seq))
          (list (string-append* (map stringify-elem
                                     ((second (platform-struct-label-framing plat)) (car seq)))))))

(define (stringify plat name seqs touched locals)
  (string-join
   (append* (append
             (list (list (string-append* (map stringify-elem
                                              ((first (platform-struct-function-framing plat)) name locals touched)))))
             (map-curry stringify-block plat (enumerate seqs))
             (list (list (string-append* (map stringify-elem
                                              ((second (platform-struct-function-framing plat)) name locals touched)))))))
   "\n"))

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

(define (stringify-exports platform exports)
  (define procs (platform-struct-export-processors platform))
  (define (stringify-one-export name-and-dataset)
    (assert (= (length name-and-dataset) 2) "Bad export section.")
    (let* ((name (car name-and-dataset))
           (dataset (second name-and-dataset))
           (uniquified-dataset (unique dataset #:cmp< (curry pair<? #:cmp< symbol<?)))
           (proc (second (assert (assoc name procs) (string-append "Expected a processor for export section: " (~a name))))))
      (proc uniquified-dataset)))
  (string-join
   (map stringify-one-export (merge-export-sets exports empty))
   "\n"))
  