#lang racket
(require plot)

(define (my-random [lower 0] [upper 1] [min 0] [max 4294967087])
  (exact->inexact (+ (* (- upper lower) (/ (random min max) (- max min 1))) lower)))

(define (transpose matrix) (apply map list matrix))
(define (matrix-add a b) (map (λ(x y) (map + x y)) a b))
(define (matrix-learn a alfa) (map (λ(x) (map (λ(y) (* (- alfa) y)) x)) a))

(define (sigmoid x) (/ 1 (+ 1 (exp (- x)))))
(define (d->sigmoid x) (define s (sigmoid x)) (* s (- 1 s)))

(define (file-lines->list path)
  (call-with-input-file path
    (λ (file)
      (for/list ([line (in-lines file)])
        (map string->number (string-split line))))))

(define dataset (file-lines->list "dataset.txt"))

(define (get-max lst)
  (apply max (map first lst)))

(define nr-zones (+ 1 (get-max dataset)))
(define dimension 2)

(define (codify target)
  (for/list ([i (in-range nr-zones)])
    (if (= i target) 1 0)))

(define (build lst)
  (for/list ([i (in-list lst)])
    (cons (codify (first i)) (map (λ(x) (exact->inexact (/ x 300))) (rest i)))))

(define train-points (build dataset))

(define N1 dimension)
(define N2 (+ 1 dimension))
(define N3 nr-zones)

(define W12 (for/list ([i (in-range N2)])
              (for/list ([j (in-range N1)])
                (my-random))))

(define Prag2 (for/list ([i (in-range N2)])
                (my-random)))

(define W23 (for/list ([i (in-range N3)])
              (for/list ([j (in-range N2)])
                (my-random))))

(define Prag3 (for/list ([i (in-range N3)])
                (my-random)))


(define network (list (list W12 Prag2) (list W23 Prag3)))

(define network2 '((((-3.731497933146584 18.276768821183357)
                     (-8.507357799726561 2.979136728297567)
                     (7.377937858604942 -7.835328035172123))
                    (-9.405265559509498
                     -3.322784720984437
                     -5.1028382510786265))
                   (((13.256613860489107
                      -12.889016641402828
                      -4.87968957839649)
                     (-26.208530174798163
                      28.07203553131441
                      -16.707583616183907)
                     (-5.872355283510901
                      -10.66629934669147
                      7.676768750421209))
                    (-1.1464375210455628
                     1.1777241928943405
                     -1.5040735113581627))))

(define (forward input layer)
  (map + (second layer) (for/list ([weights (in-list (first layer))])
                          (apply + (map * input weights)))))

(define (update-online dataset layers [t 0])
  (let loop ([clone dataset] [new-layers layers] [Error 0])
    (cond
      [(empty? clone) (list Error new-layers)]
      [else
       ;(define a 0.5)
       (define a (* 0.8 (exp (- (/ t 100)))))
       (define input (first clone))
       (define Scop (first input))
       (define nOut1 (rest input))

       (define new-W12 (caar new-layers))
       (define new-W23 (caadr new-layers))
    
       (define nOut2 (forward nOut1 (first new-layers)))
       (define Out2 (map sigmoid nOut2))
       (define d->Out2 (map d->sigmoid nOut2))
  
       (define nOut3 (forward Out2 (second new-layers)))
       (define Out3 (map sigmoid nOut3))
       (define d->Out3 (map d->sigmoid nOut3))
  
       (define E (apply + (map (λ(x y) (sqr (- x y))) Out3 Scop)))
    
       (define d->E/Prag3 (map (λ(x) (* 2 x)) (map * (map - Out3 Scop) d->Out3)))
       (define d->E/W23 (for/list ([i (in-range N3)])
                          (map (λ(x) (* (list-ref d->E/Prag3 i) x)) Out2)))

       (define d->E/Prag2 (for/list ([i (in-range N2)])
                            (* (apply + (map * (map - Out3 Scop) d->Out3 (list-ref (transpose new-W23) i)))
                               (list-ref d->Out2 i))))
       (define d->E/W12 (for/list ([i (in-range N2)])
                          (for/list ([j (in-list nOut1)])
                            (* (list-ref d->E/Prag2 i) j))))
       (loop (rest clone)
             (list (list (matrix-add (caar new-layers) (matrix-learn d->E/W12 a))
                         (map + (cadar new-layers) (map (λ(x) (* (- a) x)) d->E/Prag2)))
                   (list (matrix-add (caadr new-layers) (matrix-learn d->E/W23 a))
                         (map + (cadadr new-layers) (map (λ(x) (* (- a) x)) d->E/Prag3))))
             (+ Error E))])))

(define (train dataset layers)
  (let loop ([n 0] [new-layers layers] [Error 1000000])
    (define x (update-online dataset new-layers))
    (cond
      ;[(<= Error (expt 10 -27)) (printf "~a\n" n) (list Error new-layers)]
      [(= n 100) (list Error new-layers)]
      [else (loop (+ 1 n) (second x) (first x))])))

;(define network4 (second (train train-points network)))

;;----------------- testing ---------------------

(define all-points empty)

(for ([i (in-range -300 300 3)])
  (for ([j (in-range -300 300 3)])
    (set! all-points (cons (map (λ(x) (exact->inexact (/ x 300))) (list i j)) all-points))))

(define (test input layers)
  (map sigmoid (forward (map sigmoid (forward input (first layers))) (second layers))))

(define (decodify lst to-find)
  (for/or ([element lst] [i (in-naturals)] #:when (equal? to-find element)) i))

(define testing-set
  (for/list ([i (in-list all-points)])
    (define result (test i network2))
    (cons (decodify result (apply max result)) (map (λ(x) (* x 300)) i))))

(define (retrieve-points lst zone)
  (if (empty? lst)
      empty
      (if (= (caar lst) zone)
          (cons (cdar lst) (retrieve-points (cdr lst) zone))
          (retrieve-points (cdr lst) zone))))

(define colors (list "DarkGreen" "Navy" "Olive" "Red" "SlateGray"
                     "Turquoise" "RoyalBlue" "DeepPink" "Magenta" "Coral"))

(plot-new-window? #t)
(plot-width 600)
(plot-height 600)
(plot-x-label #f)
(plot-y-label #f)
(plot-background "AliceBlue")

(plot
 (list
  (for/list ([i (in-range nr-zones)])
    (points (retrieve-points testing-set i)
            #:x-min -300 #:x-max 300
            #:y-min -300 #:y-max 300
            #:sym 'fullcircle #:color (list-ref colors i)
            #:alpha 0.1))
  (for/list ([i (in-range nr-zones)])
    (points (retrieve-points dataset i)
            #:x-min -300 #:x-max 300
            #:y-min -300 #:y-max 300
            #:sym 'times #:color (list-ref colors i)
            #:alpha 1))))
