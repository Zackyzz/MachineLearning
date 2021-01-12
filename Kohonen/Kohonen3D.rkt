#lang racket
(require racket/gui plot)

(define (file->vector path)
  (call-with-input-file path
    (λ (file)
      (for/vector ([line (in-lines file)])
        (vector-map string->number (list->vector (string-split line)))))))

(define dataset (vector-map (λ(x) (vector-drop x 1)) (file->vector "datasetd3.txt")))

(define neurons (for/vector ([i (in-range 10)])
                  (for/vector ([j (in-range 10)])
                    (vector (random -300 300) (random -300 300) (random -300 300)))))

(define (transpose matrix)
  (for/vector ([i (vector-length (vector-ref matrix 0))])
    (for/vector ([j matrix])
      (vector-ref j i))))

(define (get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (set matrix i j what)
  (vector-set! (vector-ref matrix i) j what))

(define N 10)
(define DIMENSION 3)

(define (alpha t)
  (* 0.7 (exp (- (/ t N)))))

(define (V-rate t)
  (exact-floor (+ 1 (* 8.7 (exp (- (/ t N)))))))

(define (euclidean-distance p q)
  (sqrt  (for/fold ([sum 0])
                   ([i (in-range DIMENSION)])
           (+ sum (sqr (- (vector-ref p i) (vector-ref q i)))))))

(define (winner point)
  (define min 1000)
  (define result empty)
  (for ([i (in-range (vector-length neurons))])
    (for ([j (in-range (vector-length (vector-ref neurons 0)))])
      (define l (euclidean-distance point (get neurons i j)))
      (when (< l min)
        (set! min l)
        (set! result (list i j)))))
  result)

(define (update a point neuron)
  (for/vector ([i (in-range DIMENSION)])
    (+ (* (vector-ref neuron i) (- 1 a)) (* a (vector-ref point i)))))

(define (update-neurons t point)
  (define closest (winner point))
  (define x (first closest))
  (define y (second closest))
  (define V (V-rate t))
  (define a (alpha t))
  (for ([i (in-range (vector-length neurons))])
    (for ([j (in-range (vector-length (vector-ref neurons 0)))])
      (when (and (and (< i (+ x V)) (> i (- x V))) (and (< j (+ y V)) (> j (- y V))))
        (set neurons i j (update a point (get neurons i j)))))))

(define (go-epoch t)
  (for ([i (in-range (vector-length dataset))])
    (update-neurons t (vector-ref dataset i))))

(define main-window (new frame% [label "Kohonen"] [width 1360] [height 730] [x 0] [y 0]))
(define canvas-panel (new panel% [parent main-window]))
(define function-canvas (new canvas% [parent canvas-panel]))

(send main-window show #t)
(sleep/yield 0)

(plot-background "black")
(plot-foreground "white")
(plot-width 600)
(plot-height 600)
(plot3d-altitude 60)
(plot3d-angle 120)

(define (plot-lines)
  (let loop ([t 0])
    (define dc (send function-canvas get-dc))
    (send dc clear)
    (plot3d/dc (list
                (points3d dataset
                          #:x-min -300 #:x-max 300
                          #:y-min -300 #:y-max 300
                          #:z-min -300 #:z-max 300
                          #:sym 'pixel #:color "aqua"
                          #:alpha 1)
                (for/list ([i neurons])
                  (lines3d i
                           #:x-min -300 #:x-max 300
                           #:y-min -300 #:y-max 300
                           #:z-min -300 #:z-max 300
                           #:color "Chocolate" #:alpha 1))
                (for/list ([i (transpose neurons)])
                  (lines3d i
                           #:x-min -300 #:x-max 300
                           #:y-min -300 #:y-max 300
                           #:z-min -300 #:z-max 300
                           #:color "Chocolate" #:alpha 1)))
               dc
               0 0
               (- (send canvas-panel get-width) 0)
               (- (send canvas-panel get-height) 0))
    (go-epoch t)
    (sleep/yield 0)
    (when (> (alpha t) 0.01)
      (loop (+ 1 t)))))

(plot-lines)