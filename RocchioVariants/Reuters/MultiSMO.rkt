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

(define (normalize lst)
  (cons (first lst) (map (λ(x) (/ x 10.0)) (rest lst))))

(define train-set (take (map normalize (get-dataset (split-set trainset))) 200))
(define test-set (take (map normalize (get-dataset (split-set testset))) 100))

(define (change-class cls dataset)
  (map (λ(x) (if (= cls (first x)) (cons 1 (rest x)) (cons -1 (rest x)))) dataset))

(define classes (sort (remove-duplicates (map first train-set)) <))

(define (vectorize set)
  (for/vector ([i set])
    (vector 0 i)))

(define smo-sets
  (for/vector ([i classes])
    (vectorize (change-class i train-set))))



;------------------------------------------

(define C 2)
(define tol 0.001)
(define b 0)
(define bs
  (for/vector ([i classes])
    0))

(define (dot a b)
  (exp (* -0.1 (apply + (map (λ(x y) (sqr (- x y))) a b)))))

(define (dot2 a b)
  (sqr (apply + (map * a b))))

(define (vfirst vect)
  (vector-ref vect 0))

(define (vsecond vect)
  (vector-ref vect 1))

(define (random-j N i)
  (define rnd (random 0 N))
  (if (not (= rnd i))
      rnd
      (random-j N i)))

(define (f set X b)
  (for/fold ([sum b])
            ([i set])
    (+ sum (* (vfirst i) (first (vsecond i)) (dot X (rest (vsecond i)))))))

(define smo-set #())

(define (multiclass-SMO sets)
  (for ([i classes])
    (printf "Class: ~a\n" i)
    (set! smo-set (vector-ref smo-sets (index-of classes i)))
    (set! b (vector-ref bs (index-of classes i)))
    (define (SMO max-passes)
      (let loop ([passes 0] [iteration 0])
        (cond
          [(or (> passes max-passes) (> iteration 50))
           (list iteration (vector-length smo-set))]
          [else
           (define changes 0)
           (define N (vector-length smo-set))
           (define (outer-loop [i 0])
             (cond
               [(= i N) #t]
               [else
                (define alphai (vfirst (vector-ref smo-set i)))
                (define xi (rest (vsecond (vector-ref smo-set i))))
                (define yi (first (vsecond (vector-ref smo-set i))))
                (define Ei (- (f smo-set xi b) yi))
    
                (when (or (and (< (* Ei yi) (- tol)) (< alphai C))
                          (and (> (* Ei yi) tol) (> alphai 0)))
              
                  (define j (random-j N i))
                  #|(define j
                    (caar (sort
                           (for/list ([q 1])
                             (define rnd (random-j N i))
                             (list rnd (- (f smo-set (rest (vsecond (vector-ref smo-set rnd))) b)
                                          (first (vsecond (vector-ref smo-set rnd))))))
                           > #:key (λ(x) (abs (- Ei (cadr x)))))))|#
              
                  (define alphaj (vfirst (vector-ref smo-set j)))
                  (define xj (rest (vsecond (vector-ref smo-set j))))
                  (define yj (first (vsecond (vector-ref smo-set j))))
                  (define Ej (- (f smo-set xj b) yj))

                  (define L (if (= yi yj)
                                (max 0 (- (+ alphai alphaj) C))
                                (max 0 (- alphaj alphai))))
                  (define H (if (= yi yj)
                                (min C (+ alphai alphaj))
                                (min C (- (+ C alphaj) alphai))))
              
                  (when (not (= L H))
                    (define eta (- (* 2 (dot xi xj)) (dot xi xi) (dot xj xj)))
                    (when  (not (>= eta 0))
                      (define aj (- alphaj (/ (* yj (- Ei Ej)) eta)))
                      (when (> aj H)
                        (set! aj H))
                      (when (< aj L)
                        (set! aj L))

                      (when (not (< (abs (- alphaj aj)) tol))
                        (define ai (+ alphai (* yi yj (- alphaj aj))))
                        (vector-set! smo-set i (vector ai (cons yi xi)))
                        (vector-set! smo-set j (vector aj (cons yj xj)))

                        (set! changes (+ 1 changes))

                        (define b1 (- b Ei (* yi (- ai alphai) (dot xi xi))
                                      (* yj (- aj alphaj) (dot xi xj))))
                        (define b2 (- b Ej (* yi (- ai alphai) (dot xi xj))
                                      (* yj (- aj alphaj) (dot xj xj))))
                        (if (and (> ai 0) (< ai C))
                            (set! b b1)
                            (if (and (> aj 0) (< aj C))
                                (set! b b2)
                                (set! b (/ (+ b1 b2) 2))))))))
                (outer-loop (+ 1 i))]))
           (outer-loop 0)
           (if (= 0 changes)
               (set! passes (+ 1 passes))
               (set! passes 0))
       
           (unless (= 0 changes)
             (printf "Iteration: ~a Support Vectors: ~a\n" iteration (vector-length smo-set)))
           (define temp-set empty)
           (for ([i (in-range (vector-length smo-set))])
             (unless (= (vfirst (vector-ref smo-set i)) 0)
               (set! temp-set (cons (vector-ref smo-set i) temp-set))))
           (set! smo-set (list->vector temp-set))
           (loop passes (+ 1 iteration))])))
    (SMO 2)
    (vector-set! smo-sets (index-of classes i) smo-set)
    (vector-set! bs (index-of classes i) b)))

(multiclass-SMO smo-sets)

(printf "Number of correct predictions: ")
(for/fold ([sum 0])
          ([example test-set])
  (if (= (first example)
         (caar (sort
                (for/list ([i classes])
                  (list i (f (vector-ref smo-sets (index-of classes i))
                             (rest example) (vector-ref bs (index-of classes i)))))
                > #:key (λ(x) (second x)))))
      (+ sum 1)
      (+ sum 0)))