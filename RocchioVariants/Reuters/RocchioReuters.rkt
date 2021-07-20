#lang racket

(define trainset (file->lines "TrainingSVM1309.arff"))
(define testset (file->lines "TestingSVM1309.arff"))

;(define samples (string->number (first dataset)))
(define dimension (string->number (second trainset)))
;(define classes (string->number (third dataset)))

(define (split-set dataset) (map (λ(x) (string-split x)) (drop dataset 3)))

(define (get-sample sample)
  (let loop ([clone sample] [new-sample (make-list dimension 0)])
    (cond
      [(equal? "#" (first clone))
       (define cls (string->number (substring (second clone) 1)))
       (cons (if (not cls) 0 cls) new-sample)]
      [else
       (define coordinate (map string->number (string-split (first clone) #rx"(:)")))
       (loop (rest clone)
             (list-set new-sample (first coordinate) (second coordinate)))])))

(define (get-dataset fileset)
  (for/list ([i fileset])
    (get-sample i)))

(define train-set (get-dataset (split-set trainset)))
(define test-set (get-dataset (split-set testset)))
                              
(define decimal exact->inexact)

(define classes (sort (remove-duplicates (map first train-set)) <))

(define (euclidean-distance p q)
  (sqrt (apply + (map (λ(x y) (sqr (- x y))) p q))))

(define (get-points lst centroid)
  (if (empty? lst)
      empty
      (if (= (caar lst) centroid)
          (cons (cdar lst) (get-points (cdr lst) centroid))
          (get-points (cdr lst) centroid))))

(define (train-rocchio lst)
  (for/list ([i classes])
    (define class-points (get-points lst i))
    (map (λ(x) (decimal (/ x (length class-points)))) (apply map + class-points))))

(define trained (train-rocchio train-set))

;-------------------- TESTING ----------------------

(define (arg-min lst)
  (sort lst < #:key second))

(define (apply-rocchio centroids new-element)
  (caar (arg-min
         (for/list ([i centroids] [j classes])
           (list j (euclidean-distance i new-element))))))

(define (update-matrix r p matrix)
  (define (index r p i)
    (cond
      [(= r p i) 0]
      [(and (= r i) (not (= r p))) 1]
      [(and (= p i) (not (= r p))) 2]
      [else 3]))
  (for/list ([classX matrix] [i classes])
    (list-update classX (index r p i) add1)))

(define (get-error-matrix centroids dataset)
  (let loop ([clone dataset]
             [error-matrix (make-list (length classes) (make-list 4 0))]
             [corrects 0])
    (cond
      [(equal? empty clone) (list error-matrix corrects)]
      [else
       (define reality (caar clone))
       (define element (cdar clone))
       (define prediction (apply-rocchio centroids element))
       (loop (rest clone) (update-matrix reality prediction error-matrix)
             (if (= reality prediction) (+ 1 corrects) corrects))])))


(define testss (get-error-matrix trained test-set))
(define error-matrix (first testss))

(define (cut-digits n)
  (decimal (/ (floor (* n (expt 10 4))) (expt 10 2))))

(define results
  (for/list ([i error-matrix] [j classes])
    (define TP (first i)) ;r p
    (define FN (second i)) ;r !p
    (define FP (third i)) ;!r p
    (define TN (fourth i)) ;!r !p

    (define Accuracy (/ (+ TP TN) (+ TP FN FP TN)))
    (define Precision (/ TP (+ TP FP)))
    (define Recall (/ TP (+ TP FN)))

    (printf "Class ~a:\n" j)
    (printf "Accuracy: ~a%\n" (cut-digits Accuracy))
    (printf "Precision: ~a%\n" (cut-digits Precision))
    (printf "Recall: ~a%\n" (cut-digits Recall))
    (printf "Error matrix: ~a \n\n" i)
    (map exact->inexact (list Accuracy Precision Recall))))

(map cut-digits (map (λ(x) (/ x 14.0)) (apply map + results)))

(second testss)
(/ (second testss) 2351.0)