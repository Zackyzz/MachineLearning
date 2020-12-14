#lang racket
(require racket/gui plot)

(define (file->vector path)
  (call-with-input-file path
    (λ (file)
      (for/vector ([line (in-lines file)])
        (vector-map string->number (list->vector (string-split line)))))))

(define dataset (vector-map (λ(x) (vector-drop x 1)) (file->vector "dataset.txt")))

(define neurons (for/vector ([i (in-range -270 271 60)])
                  (for/vector ([j (in-range -270 271 60)])
                    (vector i j))))

(define (transpose matrix)
  (for/vector ([i (vector-length (vector-ref matrix 0))])
    (for/vector ([j matrix])
      (vector-ref j i))))

(define (get matrix i j)
  (vector-ref (vector-ref matrix i) j))

(define (set matrix i j what)
  (vector-set! (vector-ref matrix i) j what))

(define N 10)
(define DIMENSION 2)

(define (alpha t)
  (* 0.7 (exp (- (/ t N)))))

(define (V-rate t)
  (exact-floor (+ 1 (* 6.1 (exp (- (/ t N)))))))

(define (euclidean-distance p q)
  (sqrt  (for/fold ([sum 0])
                   ([i (in-range DIMENSION)])
           (+ sum (sqr (- (vector-ref p i) (vector-ref q i)))))))

(define (winner point)
  (define min 100000)
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
    (plot/dc (list
              (for/list ([i neurons])
                (lines i
                       #:x-min -300 #:x-max 300
                       #:y-min -300 #:y-max 300
                       #:color "Chocolate" #:alpha 1))
              (for/list ([i (transpose neurons)])
                (lines i
                       #:x-min -300 #:x-max 300
                       #:y-min -300 #:y-max 300
                       #:color "Chocolate" #:alpha 1)))
             dc
             0 0
             (- (send canvas-panel get-width) 0)
             (- (send canvas-panel get-height) 0))
    (time (go-epoch t))
    (sleep/yield 0.001)
    (when (> (alpha t) 0.01)
      (loop (+ 1 t)))))

(plot-neurons)

#|(define qt6 empty)

(for ([i (in-range (vector-length neurons))])
  (for ([j (in-range (vector-length (vector-ref neurons 0)))])
    (set! qt6 (append (list (vector->list (vector-ref (vector-ref neurons i) j))) qt6))))

(define (list->file lst path)
  (let loop ([clone lst] [out (open-output-file path #:exists 'replace)])
    (if (empty? clone)
        (close-output-port out)
        (begin
          (for ([i (in-range (length (first clone)))])
            (display (list-ref (first clone) i) out)
            (when (< (+ i 1) (length (first clone))) (display " " out)))
          (unless (empty? (rest clone)) (display "\n" out))
          (loop (rest clone) out)))))

(list->file qt6 "qt6.txt")|#