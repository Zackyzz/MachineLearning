#lang racket
(require racket/gui plot)

(define (file->vector path)
  (call-with-input-file path
    (λ (file)
      (for/vector ([line (in-lines file)])
        (vector-map string->number (list->vector (string-split line)))))))

(define dataset (vector-map (λ(x) (vector-drop x 1)) (file->vector "dataset2.txt")))

(define neurons (for/vector ([i (in-range 100)])
                  (vector 0 0)))

(define (get matrix i)
  (vector-ref matrix i))

(define (set matrix i what)
  (vector-set! matrix i what))

(define N 10)
(define DIMENSION 2)

(define (alpha t)
  (* 0.7 (exp (- (/ t N)))))

(define (V-rate t)
  (exact-floor (+ 1 (* 10.1 (exp (- (/ t N)))))))

(define (euclidean-distance p q)
  (sqrt  (for/fold ([sum 0])
                   ([i (in-range DIMENSION)])
           (+ sum (sqr (- (vector-ref p i) (vector-ref q i)))))))

(define (winner point)
  (define min 100000)
  (define result empty)
  (for ([i (in-range (vector-length neurons))])
    (define l (euclidean-distance point (get neurons i)))
    (when (< l min)
      (set! min l)
      (set! result i)))
  result)

(define (update a point neuron)
  (for/vector ([i (in-range DIMENSION)])
    (+ (* (vector-ref neuron i) (- 1 a)) (* a (vector-ref point i)))))

(define (update-neurons t point)
  (define x (winner point))
  (define V (V-rate t))
  (define a (alpha t))
  (for ([i (in-range (vector-length neurons))])
    (when (and (< i (+ x V)) (> i (- x V)))
      (set neurons i (update a point (get neurons i))))))

(define (go-epoch t)
  (for ([i (in-range (vector-length dataset))])
    (update-neurons t (vector-ref dataset i))))

(define main-window (new frame% [label "Kohonen"] [width 700] [height 700] [x 0] [y 0]))
(define canvas-panel (new panel% [parent main-window]))
(define function-canvas (new canvas% [parent canvas-panel]))

(send main-window show #t)
(plot-background-alpha 0)
(sleep/yield 0)

(define background
  (plot-bitmap (points dataset
                       #:x-min -300 #:x-max 300
                       #:y-min -300 #:y-max 300
                       #:sym 'pixel #:color "black"
                       #:alpha 1)
               #:width (- (send canvas-panel get-width) 0)
               #:height (- (send canvas-panel get-height) 0)))
(define (trim n)
  (/ (floor (* n 10000)) 10000))

(define (plot-neurons)
  (let loop ([t 0])
    (printf "Epoch ~a: α(t) = ~a & V(t) = ~a\n"
            t (trim (alpha t)) (V-rate t))
    (define dc (send function-canvas get-dc))
    (send dc clear)
    (send dc draw-bitmap background 0 0)
    (plot/dc (lines neurons
                    #:x-min -300 #:x-max 300
                    #:y-min -300 #:y-max 300
                    #:color "Chocolate" #:alpha 1)
              
             dc
             0 0
             (- (send canvas-panel get-width) 0)
             (- (send canvas-panel get-height) 0))
    (time (go-epoch t))
    (sleep/yield 0.001)
    (when (> (alpha t) 0.01)
      (loop (+ 1 t)))))

(plot-neurons)