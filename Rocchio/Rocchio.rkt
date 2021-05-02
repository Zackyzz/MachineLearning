#lang racket/gui
(require plot)

(define decimal exact->inexact)

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define (change-class dataset prev after)
  (map (λ(x) (if (= prev (car x)) (cons after (cdr x)) x)) dataset))

(define train-set (file-lines->list "training2.txt"))
(define test-set (file-lines->list "testing2.txt"))

(define classes (sort (remove-duplicates (map first train-set)) <))

(define (euclidean-distance p q)
  (sqrt (apply + (map (λ(x y) (sqr (- x y))) p q))))

(define (cosine-similarity p q)
  (decimal (/ (apply + (map * q p))
              (sqrt (* (apply + (map sqr p)) (apply + (map sqr q)))))))

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

(define (arg-min lst)
  (sort lst < #:key second))

(define (apply-rocchio centroids new-element)
  (caar (arg-min
         (for/list ([i centroids] [j classes])
           (list j (euclidean-distance i new-element))))))

(define trained (train-rocchio train-set))

(define (update-matrix r p matrix)
  (define (index r p i)
    (cond
      [(= r p i) 0]
      [(and (= r i) (not (= r p))) 1]
      [(and (= p i) (not (= r p))) 2]
      [else 3]))
  (for/list ([classX matrix] [i classes])
    (list-update classX (index r p i) add1)))

(define (test-rocchio centroids dataset)
  (let loop ([clone dataset]
             [error-matrix (make-list (length classes) (make-list 4 0))])
    (cond
      [(equal? empty clone) error-matrix]
      [else
       (define reality (caar clone))
       (define element (cdar clone))
       (define prediction (apply-rocchio centroids element))
       (loop (rest clone) (update-matrix reality prediction error-matrix))])))

(define error-matrix (test-rocchio trained test-set))

(define (cut-digits n)
  (decimal (/ (floor (* n (expt 10 4))) (expt 10 2))))

(for ([i error-matrix] [j classes])
  (define TP (first i)) ;r p
  (define FN (second i)) ;r !p
  (define FP (third i)) ;!r p
  (define TN (fourth i)) ;!r !p

  (printf "Class ~a:\n" j)
  (printf "Accuracy: ~a%\n" (cut-digits (/ (+ TP TN) (+ TP FN FP TN))))
  (printf "Precision: ~a%\n" (cut-digits (/ TP (+ TP FP))))
  (printf "Recall: ~a%\n" (cut-digits (/ TP (+ TP FN))))
  (printf "Error matrix: ~a \n\n" i))

;-------------------------------------------------------------------

(define all-points empty)

(for ([i (in-range -300 300 3)])
  (for ([j (in-range -300 300 3)])
    (set! all-points (cons (list i j) all-points))))

(define colors (list "Red" "Navy" "Olive" "DarkGreen" "Magenta"
                     "Maroon" "RoyalBlue" "DeepPink" "Coral"))

(define test (time (map (λ(x) (cons (apply-rocchio trained x) x)) all-points)))

(define (go-plot)
  (plot-background "AliceBlue")
  (plot-width 600)
  (plot-height 600)
  (plot-x-label #f)
  (plot-y-label #f)
  (plot-new-window? #t)

  (plot (list
         (for/list ([i classes])
           (points (get-points test i)
                   #:x-min -300 #:x-max 300
                   #:y-min -300 #:y-max 300
                   #:sym 'fullcircle3 #:color (list-ref colors i)
                   #:alpha 0.1))
         (for/list ([i classes])
           (points (get-points test-set i)
                   #:x-min -300 #:x-max 300
                   #:y-min -300 #:y-max 300
                   #:sym 'full6star #:color (list-ref colors i)
                   #:alpha 1))
         (for/list ([centroid trained] [i colors])
           (points (list centroid)
                   #:x-min -300 #:x-max 300
                   #:y-min -300 #:y-max 300
                   #:sym 'fullcircle8 #:color "Black"
                   #:alpha 1)))))

(go-plot)