#lang racket

(define (my-random [lower 0] [upper 1] [min 0] [max 4294967087])
  (exact->inexact (+ (* (- upper lower) (/ (random min max) (- max min 1))) lower)))

(define (transpose matrix) (apply map list matrix))
(define (matrix-add a b) (map (λ(x y) (map + x y)) a b))
(define (matrix-learn a alfa) (map (λ(x) (map (λ(y) (* (- alfa) y)) x)) a))

(define (sigmoid x) (/ 1 (+ 1 (exp (- x)))))
(define (d->sigmoid x) (define s (sigmoid x)) (* s (- 1 s)))


(define xor-set '(((0.1) 0.1 0.1)
                  ((0.9) 0.1 0.9)
                  ((0.9) 0.9 0.1)
                  ((0.1) 0.9 0.9)))
(define N1 2)
(define N2 4)
(define N3 1)

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

(define (forward input layer)
  (map + (second layer) (for/list ([weights (in-list (first layer))])
                          (apply + (map * input weights)))))

(define (go-epoch dataset layers [mode "online"] [t 0])
  (let loop ([clone dataset] [new-layers layers] [Error 0])
    (cond
      [(empty? clone) (list Error new-layers)]
      [else
       ;(define a 0.4)
       (define a (* 0.8 (exp (- (/ t 10000)))))
       (define input (first clone))
       (define Scop (first input))
       (define nOut1 (rest input))

       (define actual-layers (if (equal? mode "online") new-layers layers))
       
       (define new-W12 (caar actual-layers))
       (define new-W23 (caadr actual-layers))
    
       (define nOut2 (forward nOut1 (first actual-layers)))
       (define Out2 (map sigmoid nOut2))
       (define d->Out2 (map d->sigmoid nOut2))
  
       (define nOut3 (forward Out2 (second actual-layers)))
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
    (when (= 1 (remainder n 1000))
      (printf "Epoch ~a: Error = ~a\n" n Error))
    (define epoch (go-epoch dataset new-layers "online"))
    (cond
      [(or (<= Error (expt 10 -28)) (= n 100000)) (printf "~a\n" n) (list Error new-layers)]
      [else (loop (+ 1 n) (second epoch) (first epoch))])))

(define (test input layers)
  (map sigmoid (forward (map sigmoid (forward input (first layers))) (second layers))))

(define trained-network (time (train xor-set network)))

(first trained-network)
(test '(0.1 0.1) (second trained-network))
(test '(0.1 0.9) (second trained-network))
(test '(0.9 0.1) (second trained-network))
(test '(0.9 0.9) (second trained-network))