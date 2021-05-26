#lang racket
(require plot)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define (change-class dataset prev after)
  (map (λ(x) (if (= prev (car x)) (cons after (cdr x)) x)) dataset))

(define (normalize lst)
  (cons (first lst) (map (λ(x) (/ x 300.0)) (rest lst))))

(define train-set
  (take (map normalize (change-class (change-class (change-class (file-lines->list "train-xor.txt") 2 1) 3 0) 0 -1)) 1000))

(define test-set
  (drop (map normalize (change-class (change-class (change-class (file-lines->list "train-xor.txt") 2 1) 3 0) 0 -1)) 9500))


#|(define train-set
  (take (map normalize (change-class (file-lines->list "dataSMO2.txt") 0 -1)) 3000))

(define test-set
  (drop (map normalize (change-class (file-lines->list "dataSMO2.txt") 0 -1)) 9000))
|#

(define (dot a b)
  (apply + (map * a b)))

(define (vectorize set)
  (for/vector ([i set])
    (vector 1 i)))

(define smo-set (vectorize train-set))

(define (vfirst vect)
  (vector-ref vect 0))

(define (vsecond vect)
  (vector-ref vect 1))

(define C 1)
(define tol 0.001)
(define b 0)

(define N (vector-length smo-set))

(define (f set X)
  (for/fold ([sum b])
            ([i set])
    (+ sum (* (vfirst i) (first (vsecond i)) (dot X (rest (vsecond i)))))))

(define (random-j N i)
  (define rnd (random 0 N))
  (if (not (= rnd i))
      rnd
      (random-j N i)))

(define (vector-delete v i)
  (vector-append (vector-take v i) (vector-drop v (+ i 1))))

(define (SMO it)
  (let loop ([passes 0])
    (cond
      [(> passes it) passes]
      [else
       (define changes 0)
       (define N (vector-length smo-set))
       (define (outer-loop [i 0])
         (cond
           [(= i N) (printf "~a\n" passes)]
           [else
            (define alphai (vfirst (vector-ref smo-set i)))
            (define xi (rest (vsecond (vector-ref smo-set i))))
            (define yi (first (vsecond (vector-ref smo-set i))))
            (define Ei (- (f smo-set xi) yi))
    
            (when (or (and (< (* Ei yi) (- tol)) (< alphai C))
                      (and (> (* Ei yi) tol) (> alphai 0)))
      
              (define j (random-j N i))
              (define alphaj (vfirst (vector-ref smo-set j)))
              (define xj (rest (vsecond (vector-ref smo-set j))))
              (define yj (first (vsecond (vector-ref smo-set j))))
              (define Ej (- (f smo-set xj) yj))

              (define L (if (= yi yj)
                            (max 0 (- (+ alphai alphaj) C))
                            (max 0 (- alphaj alphai))))
              (define H (if (= yi yj)
                            (min C (+ alphai alphaj))
                            (min C (- (+ C alphaj) alphai))))
              (when (not (= L H))
                (define eta (- (* 2 (dot xi xj)) (dot xi xi) (dot xj xj)))
                (when  (not (>= eta -0.01))
                  (define aj (- alphaj (/ (* yi (- Ei Ej)) eta)))
                  (when (> aj H)
                    (set! aj H))
                  (when (< aj L)
                    (set! aj L))

                  (when (not (< (abs (- alphaj aj)) tol))
                    (define ai (+ alphai (* yi yj (- alphaj aj))))
                    (vector-set! smo-set i (vector ai (cons yi xi)))
                    (vector-set! smo-set j (vector aj (cons yj xj)))

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
       (time (outer-loop 0))
       (define fake empty)
       (for ([i (in-range (vector-length smo-set))])
         (unless (= (vfirst (vector-ref smo-set i)) 0)
           (set! fake (cons (vector-ref smo-set i) fake))))
       (set! smo-set (list->vector fake))
       (printf "it: ~a\n" (vector-length smo-set))
       (loop (+ 1 passes))])))

(SMO 100)

(printf "\nTesting:\n")
(for/fold ([sum 0])
          ([i test-set])
  (if (= (first i) (if (negative? (f smo-set (rest i))) -1 1))
      (+ sum 1)
      (begin (printf "~a\n" i) (+ sum 0))))
