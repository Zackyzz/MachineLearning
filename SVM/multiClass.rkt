#lang racket
(require plot)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define (normalize lst)
  (cons (first lst) (map (λ(x) (/ x 300.0)) (rest lst))))

(define (change-class cls dataset)
  (map (λ(x) (if (= cls (first x)) (cons 1 (rest x)) (cons -1 (rest x)))) dataset))

(define train-set
  (take (map normalize (file-lines->list "training2.txt")) 5000))

(define test-set
  (drop (map normalize (file-lines->list "training2.txt")) 5000))

(define classes (sort (remove-duplicates (map first train-set)) <))

(define (dot2 a b [f sqr])
  (f (apply + (map * a b))))

(define (dot a b)
  (exp (* -1.1 (apply + (map (λ(x y) (sqr (- x y))) a b)))))

(define (vectorize set)
  (for/vector ([i set])
    (vector 0 i)))

(define smo-sets
  (for/vector ([i classes])
    (vectorize (change-class i train-set))))

(define (vfirst vect)
  (vector-ref vect 0))

(define (vsecond vect)
  (vector-ref vect 1))

(define (f set X [bias b])
  (for/fold ([sum bias])
            ([i set])
    (define xy (vsecond i))
    (+ sum (* (vfirst i) (first xy) (dot X (rest xy))))))

(define N (length train-set))
(define C 0.3)
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
      (printf "Iteration: ~a Changed: ~a \n"
              iteration num-changed)
      (cond
        [(and (> iteration 1) (= num-changed 0)) iteration]
        [(not (or (= examine-all 1) (> num-changed 0))) iteration]
        [else
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
    (vector-set! smo-sets (index-of classes i) simplifier)
    (vector-set! bs (index-of classes i) b)
    (set! simplifier empty)))

(define (examine-example i)
  (define done 0)
  (define alphai (vfirst (vector-ref smo-set i)))
  (define xyi (vsecond (vector-ref smo-set i)))
  (define xi (rest xyi))
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
          [(or (= j (/ N 10)) (= done 1)) done]
          [else
           (set! done (take-step i (random j N) (list alphai xi yi Ei)))
           (loop (+ 1 j))]))))
  done)

(define (take-step i j prevs)
  (define alphai (first prevs))
  (define xi (second prevs))
  (define yi (third prevs))
  (define Ei (fourth prevs))
  
  (define alphaj (vfirst (vector-ref smo-set j)))
  (define xyj (vsecond (vector-ref smo-set j)))
  (define xj (rest xyj))
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
        (vector-set! smo-set i (vector ai (cons yi xi)))
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

(multiclass-SMO)

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

(define all-points
  (apply append
         (for/list ([i (in-range -300 300 3)])
           (for/list ([j (in-range -300 300 3)])
             (map (λ(x) (/ x 300)) (list i j))))))

(define colors (list "Red" "Navy" "Olive" "DarkGreen" "Coral"
                     "Maroon" "RoyalBlue" "DeepPink"))

(define test-all
  (for/list ([example all-points])
    (cons (caar (sort
                 (for/list ([i classes])
                   (list i (f (vector-ref smo-sets (index-of classes i))
                              example (vector-ref bs (index-of classes i)))))
                 > #:key (λ(x) (second x))))
          example)))

(define (get-points lst class)
  (if (empty? lst)
      empty
      (if (= (caar lst) class)
          (cons (cdar lst) (get-points (cdr lst) class))
          (get-points (cdr lst) class))))

(define (go-plot)
  (plot-background "AliceBlue")
  (plot-width 600)
  (plot-height 600)
  (plot-x-label #f)
  (plot-y-label #f)
  (plot-new-window? #t)

  (plot (list
         (for/list ([i classes])
           (points (get-points test-all i)
                   #:x-min -1 #:x-max 1
                   #:y-min -1 #:y-max 1
                   #:sym 'fullcircle4 #:color (list-ref colors (index-of classes i))
                   #:alpha 0.1))
         (for/list ([i classes])
           (points (get-points train-set i)
                   #:x-min -1 #:x-max 1
                   #:y-min -1 #:y-max 1
                   #:sym 'full7star #:color (list-ref colors (index-of classes i))
                   #:alpha 1)))))

(go-plot)
