
; (square 2)

; (square (square 3))

; (+ (square x) (square y))

; (define (f a)
;     (sum-of-squares (+ a 1) (* a 2)))

; ; conditional expressions and predicates

; (define (abs x)
;     (cond ((> x 0) x)
;     ((= x 0) 0)
;     ((< x 0) (-x))
;     ))

; (define (abs2 x)
;     (cond ((> x 0) x)
;     (else -x)
;     )
;     )

; (define (abs1 x)
;     (if (< x 0)
;     (-x)
;     x
;     )
; )

; ; exercise 1.2

; (/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5)))))) (* 3 (* (- 6 2)(- 2 7))))

; ; 1.3
(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (square x)
    (* x x))

(define (square-of-larger-two a b c)
    (cond ((and (> a b) (> b c)) (sum-of-squares a b))
    ((and (> b a) (> c a)) (sum-of-squares c b))
    ((and (> a b) (> c a)) (sum-of-squares a c)))

)

(square-of-larger-two 4 5 6)

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)
    )
)

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))


(define (good-enough? previous-guess guess)
    (< (abs (/ (- guess previous-guess) guess) 0.00000001)))

(define (cbrt x)
    (cbrt-iter 1.0 x))

(define (cbrt-iter guess x)
    (if (good-enough? guess x)
    guess
    (cbrt-iter (improve guess x) x)
    ))

(define (improve guess x)
    (/ (+ (/ x (square guess) (* 2 guess))) 3))

(define (count-change amount)
    (cc amount 5))

((define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
    ((< amount 0) 0)
    ((= kinds-of-coins 0) 0)
    (else
    (+ (cc amount (- kinds-of-coins 1))
    (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
    )
    )
    )))

(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
    ((= kinds-of-coins 2) 5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)
    ))

; 1.11
; recursive
(define (f n)
    (cond (< n 3) (n)
        (else (+ 
                (f (- n 1)) 
                (* 2 (f (- n 2))) 
                (* 3 (f (- n 3))))
    )))

; iterative
(define (f n)
    (
    (define (f-i a b c count)
        (cond ((< n 3) n)
            ((<= count 0) a))
            (else (f-i (+ a (* 2 b) (* 3 c))) a b (- count 1))
        
        )
    )) (f-i 2 1 0 (- n 2))

; 1.12

(define (pascal row col)
    (cond ((= row 1) 1)
          ((= row 2) 2)
          ((= col 1) 1)
          ((= col row) 1)
     (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))
    )))

; 1.16

(define (fast-expt b n)
    (cond ((= n 0) 1)
        ((= n 1) b)
        ((is-even? n) (fast-expt (* b b) (/ n 2))) 
        (else (fast-expt b (- n 1)))
    ))

(define (iter-fast-expt b n)
    ((define (iter N B A)
        (cond ((= 0 N) A)
              ((even? N) (iter (/ N 2) (square B) A)
              (else (iter (- N 1) B (* B A)))
              )
        (iter n b 1)
        ))))

(define (double num)
    (+ num num))

(define (halve num)
    (floor (/ num 2))
)

(define (* a b)
    (cond ((or (= b 0) (= a 0)) 0)
          ((even? b) (double (* a (halve b))))
          (else (+ a (* a (- b 1))))
    ))

(define (* a b)
    (define (iter accumulator a b)
        (cond ((= b 0) accumulator)
            ((even? b) (iter accumulator (double a) (halve b)))
            (else (iter (+ accumulator a) a (- b 1)))
        )

    )
    (iter 0 a b)
    )


; euclid's algorithm gcd

(define (gcd a b)       
    (if (= b 0)
    a
    (gcd b (remainder a b))))

; lame's theorem: if euclid's algo requires k steps to compute gcd 
; of some pair, then the smaller number in the pair must be greater
; than or equal to the kth fibonacci number

; let n be the smaller of the two inputs
; if process takes k steps, we must have
; n >= fib(k) ~ phi^k / sqrt 5
; number of steps k grows as the log of n. hence order of growth is log n

;Miller-Rabin test cannot be fooled
;“Numbers that fool the Fermat test are called Carmichael numbers,”
