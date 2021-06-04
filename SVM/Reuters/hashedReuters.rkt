#lang racket

(define trainset (file->lines "TrainingSVM1309.arff"))
(define testset (file->lines "TestingSVM1309.arff"))

;(define samples (string->number (first dataset)))
(define dimension (string->number (second trainset)))
;(define classes (string->number (third dataset)))

(define (split-set dataset) (map (λ(x) (string-split x)) (drop dataset 3)))

(define (get-sample sample [constant 70.0])
  (let loop ([clone sample] [new-sample (make-hash)])
    (cond
      [(equal? "#" (first clone))
       (define cls (string->number (substring (second clone) 1)))
       (list (if (not cls) 0 cls) new-sample)]
      [else
       (define coordinate (map string->number (string-split (first clone) #rx"(:)")))
       (hash-set! new-sample (first coordinate) (/ (second coordinate) constant))
       (loop (rest clone) new-sample)])))

(define (get-dataset fileset)
  (for/list ([i fileset])
    (get-sample i)))

(define train-set (take (get-dataset (split-set trainset)) 3000))
(define test-set (get-dataset (split-set testset)))

;;-------------------------------------------------

(define (change-class cls dataset)
  (map (λ(x) (if (= cls (first x)) (cons 1 (rest x)) (cons -1 (rest x)))) dataset))

(define classes (sort (remove-duplicates (map first train-set)) <))
classes

(define (vectorize set)
  (for/vector ([i set])
    (vector 0 i)))

(define smo-sets
  (for/vector ([i classes])
    (vectorize (change-class i train-set))))

(define (dot3 x y [f sqr])
  (f (for/fold ([sum 0])
               ([(k v) (in-hash x)])
       (+ sum (* (hash-ref y k 0) v)))))

(define (dot u v)
  (exp (* -0.5
          (let ((indices (remove-duplicates (append (hash-keys u) (hash-keys v)))))
            (apply + (map (lambda (i) (let ((x (hash-ref u i 0))
                                            (y (hash-ref v i 0)))
                                        (sqr (- x y))))
                          indices))))))

(define (random-j i N)
  (define rnd (random 0 N))
  (if (not (= rnd i))
      rnd
      (random-j i N)))

(define (vfirst vect)
  (vector-ref vect 0))

(define (vsecond vect)
  (vector-ref vect 1))

(define (f set X [bias b])
  (for/fold ([sum bias])
            ([i set])
    (define xy (vsecond i))
    (+ sum (* (vfirst i) (first xy) (dot X (second xy))))))

(define N (length train-set))
(define C 0.5)
(define tol 0.001)
(define b 0)
(define bs
  (for/vector ([i classes])
    0))

(define simplifier empty)
(define smo-set #())

(define (multiclass-SMO)
  (for ([i classes])
    (printf "Class: ~a\n" i)
    (set! smo-set (vector-ref smo-sets (index-of classes i)))
    (set! b (vector-ref bs (index-of classes i)))
    
    (define (main [num-changed 0] [examine-all 1] [iteration 0])
      (printf "Iteration: ~a ~a Changed: ~a \n"
              iteration (vector-length smo-set) num-changed)
      (cond
        [(and (> iteration 1) (= num-changed 0)) iteration]
        [(not (or (= examine-all 1) (> num-changed 0))) iteration]
        [else
         (set! N (vector-length smo-set))
         (set! num-changed 0)
         (if (= 1 examine-all)
             (for ([i N])
               (set! num-changed (+ num-changed (examine-example i))))
             (for ([i N])
               (define alphai (vfirst (vector-ref smo-set i)))
               (when (and (> alphai 0) (< alphai C))
                 (set! num-changed (+ num-changed (examine-example i))))))
         
      
         (if (= 1 examine-all)
             (set! examine-all 0)
             (when (= 0 num-changed)
               (set! examine-all 1)))
         (main num-changed examine-all (+ 1 iteration))]))

    (time (main))
    (define temp empty)
    (for ([i smo-set])
      (unless (= (vfirst i) 0)
        (set! temp (cons i temp))))
    (set! smo-set (list->vector temp))
    (printf "~a ~a\n" (vector-length smo-set) (length simplifier))
         
    (vector-set! smo-sets (index-of classes i) smo-set)
    (vector-set! bs (index-of classes i) b)
    (set! simplifier empty)))

(define (examine-example i)
  (define done 0)
  (define alphai (vfirst (vector-ref smo-set i)))
  (define xyi (vsecond (vector-ref smo-set i)))
  (define xi (second xyi))
  (define yi (first xyi))
  (define Ei (- (f simplifier xi b) yi))

  (when (or (and (< (* Ei yi) (- tol)) (< alphai C))
            (and (> (* Ei yi) tol) (> alphai 0)))

    (define non-bound empty)
    (for ([i smo-set] [index N])
      (unless (or (= (vfirst i) 0) (= (vfirst i) C))
        (set! non-bound (cons (list index i) non-bound))))
    
    (when (= done 0)
      (let loop ([clone (shuffle non-bound)])
        (cond
          [(or (equal? empty clone) (= done 1)) done]
          [else
           (define j (caar clone))
           (define setj (cadar clone))
           (define alphaj (vfirst setj))
           (set! done (take-step i j (list alphai xi yi Ei)))
           (loop (rest clone))])))
    
    #|(when (= done 0)
      (let loop ([j 0])
        (cond
          [(or (= j N) (= done 1)) done]
          [else
           (define alphaj (vfirst (vector-ref smo-set j)))
           (when (and (> alphaj 0) (< alphaj C))
             (set! done (take-step i j (list alphai xi yi Ei))))
           (loop (+ 1 j))])))|#

    (when (= done 0)
      (let loop ([j 0])
        (cond
          [(or (= j N) (= done 1)) done]
          [else
           (set! done (take-step i (random-j i N) (list alphai xi yi Ei)))
           (loop (+ 1 j))]))))
  done)

(define (take-step i j prevs)
  (define alphai (first prevs))
  (define xi (second prevs))
  (define yi (third prevs))
  (define Ei (fourth prevs))
  
  (define alphaj (vfirst (vector-ref smo-set j)))
  (define xyj (vsecond (vector-ref smo-set j)))
  (define xj (second xyj))
  (define yj (first xyj))
  (define Ej (- (f simplifier xj b) yj))

  (define L (if (= yi yj)
                (max 0 (- (+ alphai alphaj) C))
                (max 0 (- alphaj alphai))))
  (define H (if (= yi yj)
                (min C (+ alphai alphaj))
                (min C (- (+ C alphaj) alphai))))
  
  (define returner 0)
  (unless (or (= i j) (= L H))
    (define eta (- (* 2 (dot xi xj)) (dot xi xi) (dot xj xj)))
    (unless (>= eta 0)
      (define aj (- alphaj (/ (* yj (- Ei Ej)) eta)))
      (when (> aj H)
        (set! aj H))
      (when (< aj L)
        (set! aj L))

      (unless (< (abs (- alphaj aj)) tol)
        (define ai (+ alphai (* yi yj (- alphaj aj))))
        (vector-set! smo-set i (vector ai (list yi xi)))
        (vector-set! smo-set j (vector aj xyj))

        (set! simplifier empty)
        (for ([i smo-set])
          (unless (= (vfirst i) 0)
            (set! simplifier (cons i simplifier))))
        (set! returner 1)

        (define b1 (- b Ei (* yi (- ai alphai) (dot xi xi))
                      (* yj (- aj alphaj) (dot xi xj))))
        (define b2 (- b Ej (* yi (- ai alphai) (dot xi xj))
                      (* yj (- aj alphaj) (dot xj xj))))
        (if (and (> ai 0) (< ai C))
            (set! b b1)
            (if (and (> aj 0) (< aj C))
                (set! b b2)
                (set! b (/ (+ b1 b2) 2)))))))
  returner)

(time (multiclass-SMO))

(printf "Number of correct predictions: ")
(time (for/fold ([sum 0])
                ([example test-set])
        (if (= (first example)
               (caar (sort
                      (for/list ([i classes])
                        (list i (f (vector-ref smo-sets (index-of classes i))
                                   (second example) (vector-ref bs (index-of classes i)))))
                      > #:key (λ(x) (second x)))))
            (+ sum 1)
            (+ sum 0))))