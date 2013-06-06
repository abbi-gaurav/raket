#lang slideshow
(define c (circle 10))

(define (filled-square n)
  (filled-rectangle n n))

(define (four-p p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
  (let*  ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [four-checker (checker rp bp)]
         [16-checker (four-p four-checker)])
         (four-p 16-checker)))


(define (series mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20)))

(define series2 
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))
  )
)

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))

(define (rainbow p)
  (map
   (lambda (color)
     (colorize p color))
   (list 
    "violet" "indigo" "blue" 
    "green" "yellow" "orange" "red")))

(require slideshow/code)
(define-syntax pict+code
  (syntax-rules()
    [(pict+code expr)
     (hc-append 10 expr (code expr))]))
(provide rainbow filled-square)

(define (sum2 n total)
    (cond
      [(= 1 n) (+ total 1)]
      [else (sum2 (- n 1) (+ total n))]))
(define (sum n)
             (sum2 n 0))

(require racket/class
         racket/gui/base)
(define f (new frame% [label "Learn Racket"]
                      [width 300]
                      [height 300]
                      [alignment '(center center)]))

(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas%[parent f]
               [style '(border)]
               [paint-callback(lambda (self dc)
                                (drawer dc 0 0))])))
(define (double v)
  ((if(string? v) string-append +) v v))

(define (twice f v) 
    (f(f v)))

(define (make-add-suffix s2)
    (lambda (s) (string-append s s2)))

(define louder (make-add-suffix "!"))


(define x_vs_o(let ([x (random 7)]
         [o (random 7)])
    (cond
      [(> x o) "X wins"]
      [(> o x) "o wins"]
      [else "cat's game"])))

(define x_vs_o_with_margin(
 let* (
       [x (random 7)]
       [o (random 7)]
       [diff (number->string(abs (- x o )))])
  (cond
    [(> x o) (string-append "X wins by " diff)]
    [(> o x) (string-append "O wins by " diff)]
    [else "its a tie"])))

(define (map_test input)
  (map (lambda (f v) (f v))
       (list sqrt sqr)
       input))


 (foldl (lambda (elem v)
           (+ v (* elem elem)))
         0
         '(1 2 3))
 
 (define (my-length lst)
   (cond
     [(empty? lst) 0]
     [else (+ 1 (my-length (rest lst)))]))
 
 (define (my-map f lst)
   (cond
     [(empty? lst) empty]
     [else (cons (f (first lst))
                 (my-map f (rest lst)))]))
 
 (define (my-length-tail lst)
   (define (itr lst len)
     (cond
       [(empty? lst) len]
       [else (itr (rest lst) (+ len 1))]))
   (itr lst 0))
 
 (define (my-map-iter f lst)
   (for/list ([i lst])
     (f i)))
 
 (define (remove-cons-dups lst)
   (cond
     [(empty? lst) empty]
     [(empty? (rest lst)) lst]
     [else
      (let ([i (first lst)])
        (if (equal? i (first (rest lst)))
            (remove-cons-dups (rest lst))
            (cons i (remove-cons-dups (rest lst)))))]))
 
 
 (define (sigma f a b)
   (if (= a b) 
       0
       (+ (f a) (sigma f (+ a 1) b))))