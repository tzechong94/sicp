
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

