#lang racket/gui
(require plot)

(define N 300)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define dataset (map rest (file-lines->list "dataset.txt")))

(define centroids
  (build-list (random 2 10) (λ _ (list (random (- N) N) (random (- N) N)))))

(define DIMENSION (length (first centroids)))

(define colors (list "Red" "Navy" "Olive" "DarkGreen" "SlateGray"
                     "Maroon" "RoyalBlue" "DeepPink" "Magenta" "Coral"))

(define (euclidean-distance p q)
  (sqrt (for/fold ([sum 0])
                  ([i (in-range DIMENSION)])
          (+ sum (sqr (- (list-ref p i) (list-ref q i)))))))

(define (manhattan-distance p q)
  (for/fold ([sum 0])
            ([i (in-range DIMENSION)])
    (+ sum (abs (- (list-ref p i) (list-ref q i))))))

(define (label-points lst centroids)
  (define (similar-points point)
    (let loop ([n 0] [min (+ 1 (* 2 (sqrt 2) N))] [result empty] [clone centroids])
      (cond
        [(equal? empty clone) result]
        [(< (euclidean-distance point (first clone)) min)
         (loop (+ 1 n) (euclidean-distance point (first clone))
               (append (list n) point) (rest clone))]
        [else (loop (+ 1 n) min result (rest clone))])))
  (map (λ (x) (similar-points x)) lst))

(define (get-points lst centroid)
  (if (empty? lst)
      empty
      (if (= (caar lst) centroid)
          (cons (cdar lst) (get-points (cdr lst) centroid))
          (get-points (cdr lst) centroid))))

(define (gravity-center lst)
  (map (λ (x) (exact->inexact (/ x (length lst))))
       (let loop ([lista (build-list DIMENSION (λ _ 0))] [clone lst])
         (cond
           [(equal? empty clone) lista]
           [else (loop (map + lista (first clone)) (rest clone))]))))

(define (new-centroids lst centroids)
  (for/list ([i (in-range (length centroids))])
    (define points-near-centroid (get-points lst i))
    (if (equal? empty points-near-centroid)
        (list-ref centroids i)
        (gravity-center points-near-centroid))))

(define (new-cost lst centroids)
  (for/fold ([sum 0])
            ([i (in-range (length lst))])
    (define centroid (first (list-ref lst i)))
    (+ sum (euclidean-distance (rest (list-ref lst i)) (list-ref centroids centroid)))))

(define (kMeans lst centroids [cost 0] [n 1])
  (define clone (label-points lst (last centroids)))
  (define new-centers (new-centroids clone (last centroids)))
  (define epoch-cost (new-cost clone new-centers))
  (printf "Epoch ~a: \nThe new centers are \n~a \nThe cost is \n~a \n" n new-centers epoch-cost)
  (cond
    [(= cost epoch-cost) centroids]
    [else (kMeans lst (append centroids (list new-centers)) epoch-cost (+ 1 n))]))

(define all-centroids (kMeans dataset (list centroids)))

(define main-window (new frame% [label "kMeans"] [width 700] [height 700] [x 0] [y 0]))
(define canvas-panel (new panel% [parent main-window]))
(define function-canvas (new canvas% [parent canvas-panel]))

(send main-window show #t)
(sleep/yield 0)

(plot-background "AliceBlue")
(plot-width 600)
(plot-height 600)
(plot-x-label #f)
(plot-y-label #f)

(plot/dc (list
          (axes 0 0 #:x-ticks? #f #:y-ticks? #f)
          (points dataset
                  #:x-min -300 #:x-max 300
                  #:y-min -300 #:y-max 300
                  #:sym 'pixel #:color "Black"
                  #:alpha 0.6)
          (for/list ([j (in-range (length (first all-centroids)))])
            (points (list (list-ref (first all-centroids) j))
                    #:x-min -300 #:x-max 300
                    #:y-min -300 #:y-max 300
                    #:sym 'fullcircle8 #:color (list-ref colors j)
                    #:alpha 1)))
         (send function-canvas get-dc)
         0 0
         (- (send canvas-panel get-width) 0)
         (- (send canvas-panel get-height) 0))
(sleep/yield 1.5)

(define (plot-points lst list-of-centroids)
  (for ([i (in-range (length list-of-centroids))])
    (define actual-centers (list-ref all-centroids i))
    (define pointsuri (label-points lst actual-centers))
    (plot/dc (list
              (axes 0 0 #:x-ticks? #f #:y-ticks? #f)
              (for/list ([j (in-range (length actual-centers))])
                (points (get-points pointsuri j)
                        #:x-min -300 #:x-max 300
                        #:y-min -300 #:y-max 300
                        #:sym 'pixel #:color (list-ref colors j)
                        #:alpha 0.5))
              (for/list ([j (in-range (length actual-centers))])
                (points (list (list-ref actual-centers j))
                        #:x-min -300 #:x-max 300
                        #:y-min -300 #:y-max 300
                        #:sym 'fullcircle8 #:color (list-ref colors j)
                        #:alpha 1)))
             (send function-canvas get-dc)
             0 0
             (- (send canvas-panel get-width) 0)
             (- (send canvas-panel get-height) 0))
    (sleep/yield 0.7)))

(plot-points dataset all-centroids)

(define all-points empty)

(for ([i (in-range -300 300 2)])
  (for ([j (in-range -300 300 2)])
    (set! all-points (cons (list i j) all-points))))

(set! all-points (label-points (reverse all-points) (last all-centroids)))
(plot/dc (list
          (axes 0 0 #:x-ticks? #f #:y-ticks? #f)
          (for/list ([q (in-range (length (last all-centroids)))])
            (points (get-points all-points q)
                    #:x-min -300 #:x-max 300
                    #:y-min -300 #:y-max 300
                    #:sym 'circle #:color (list-ref colors q)
                    #:alpha 0.1))
          (for/list ([j (in-range (length (last all-centroids)))])
            (points (get-points (label-points dataset (last all-centroids)) j)
                    #:x-min -300 #:x-max 300
                    #:y-min -300 #:y-max 300
                    #:sym 'pixel #:color (list-ref colors j)
                    #:alpha 0.8))
          (for/list ([j (in-range (length (last all-centroids)))])
            (points (list (list-ref (last all-centroids) j))
                    #:x-min -300 #:x-max 300
                    #:y-min -300 #:y-max 300
                    #:sym 'fullcircle8 #:color (list-ref colors j)
                    #:alpha 1)))
         (send function-canvas get-dc)
         0 0
         (- (send canvas-panel get-width) 0)
         (- (send canvas-panel get-height) 0))