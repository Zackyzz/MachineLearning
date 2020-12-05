#lang racket
(require plot)

(define N 300)
(define DIMENSION 2)
(define zones2
  '(((180 10) (220 10))
    ((-100 15) (110 5))
    ((210 5) (-150 20))))

(define zones
  (build-list (random 3 10)
              (λ _  (for/list ([i (in-range DIMENSION)])
                      (list (random -250 250) (random 5 30))))))

(define colors (list "Red" "Navy" "Olive" "DarkGreen" "SlateGray"
                     "Turquoise" "RoyalBlue" "DeepPink" "Magenta" "Coral"))

(define (Gauss m σ x)
  (exp (- (/ (sqr (- m x)) (* 2 (sqr σ))))))

(define (generate M S)
  (define rnd (random (- N) N))
  (if (> (Gauss M S rnd) (/ (random 0 5001) 5000))
      rnd
      (generate M S)))

(define (generate-list n)
  (define rnd (random 0 (length zones)))
  (define random-zone (list-ref zones rnd))
  (if (= n 0)
      empty
      (cons (cons rnd
                  (for/list ([i (in-range DIMENSION)])
                    (generate (car (list-ref random-zone i)) (cadr (list-ref random-zone i)))))
            (generate-list (- n 1)))))

(define (retrieve-points lst zone)
  (if (empty? lst)
      empty
      (if (= (caar lst) zone)
          (cons (cdar lst) (retrieve-points (cdr lst) zone))
          (retrieve-points (cdr lst) zone))))

(define generated-points (generate-list 10000))

(plot-new-window? #t)
(plot-width 600)
(plot-height 600)
(plot-x-label #f)
(plot-y-label #f)
(plot-background "AliceBlue")

(plot
 (list
  (axes 0 0 #:x-ticks? #f #:y-ticks? #f)
  (for/list ([i (in-range (length zones))])
    (points (retrieve-points generated-points i)
            #:x-min -300 #:x-max 300
            #:y-min -300 #:y-max 300
            #:sym 'pixel #:color (list-ref colors i)
            #:alpha 1))))

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

(list->file generated-points
            "C:\\Users\\desktop\\Desktop\\AnIV\\MachineLearning\\generatingDataset\\dataset2.txt")