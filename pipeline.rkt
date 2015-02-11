#lang racket

(require "utilities.rkt")

(provide empty-pipeline pipe-def pipe-sched pipe-run pipe-run-sched pipe-run-sched-single)

(struct pipeline-transformer (function input-types output-type) #:inspector #f)
(struct pipeline-entry (type value) #:inspector #f)
(struct pipeline (transformers) #:inspector #f)

(define empty-pipeline (pipeline empty))

(define-syntax-rule (pipe-def cur-pipeline (platform input-type ... output-type) code ...)
  (pipeline (cons (pipeline-transformer (lambda (platform input-type ...) code ...)
                                        (list 'input-type ...) 'output-type)
                  (pipeline-transformers cur-pipeline))))

(define (recur<? a b)
  (if (empty? a)
      (if (empty? b)
          #f
          #t)
      (if (empty? b)
          #f
          (if (eq? (car a) (car b))
              (recur<? (cdr a) (cdr b))
              (symbol<? (car a) (car b))))))

(define (transf<? a b)
  (recur<? (cons (pipeline-transformer-output-type a) (pipeline-transformer-input-types a))
           (cons (pipeline-transformer-output-type b) (pipeline-transformer-input-types b))))

(define (get-outputting-transformer transformers output-type)
  (assert (not (empty? transformers)) "No transformer outputs type: " output-type)
  (if (eq? (pipeline-transformer-output-type (car transformers)) output-type)
      (car transformers)
      (get-outputting-transformer (cdr transformers) output-type)))

(define (uniquify-schedule schedule)
  (cond ((empty? schedule) schedule)
        ((member (car schedule) (cdr schedule)) (uniquify-schedule (cdr schedule)))
        (else (cons (car schedule) (uniquify-schedule (cdr schedule))))))

(define (pipe-sched pipeline input-type output-type)
  (reverse (uniquify-schedule
            (let iter ((remain output-type))
              (if (eq? remain input-type)
                  empty
                  (let ((last-transformer (get-outputting-transformer (pipeline-transformers pipeline) remain)))
                    (cons last-transformer (append* (map iter (pipeline-transformer-input-types last-transformer))))))))))

(define (do-nothing . a) (void))

(define (pipe-run platform known remaining #:reporter (reporter do-nothing))
  (if (empty? remaining)
      known
      (let* ((wanted (car remaining))
             (others (cdr remaining))
             (wanted-arguments (pipeline-transformer-input-types wanted))
             (wanted-type (pipeline-transformer-output-type wanted))
             (arguments (map (lambda (arg) (second (assoc arg known))) wanted-arguments)))
        (reporter wanted-type (void))
        (let ((result (apply (pipeline-transformer-function wanted) (cons platform arguments))))
          (reporter wanted-type result)
          (pipe-run platform (cons (list wanted-type result) known) others #:reporter reporter)))))

(define (pipe-run-sched platform pipeline input input-type output-type #:reporter (reporter do-nothing))
  (reporter input-type (void))
  (reporter input-type input)
  (pipe-run platform (list (list input-type input)) (pipe-sched pipeline input-type output-type) #:reporter reporter))

(define (pipe-run-sched-single platform pipeline input input-type output-type)
  (second (assoc output-type (pipe-run-sched platform pipeline input input-type output-type))))
