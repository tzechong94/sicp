
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

;1.29 simpson's rule

(define (cube x)
    (* x x x))


(define (inc n)
    (+ n 1))

(define (simpson f a b n)
    (define h (/ (- b a) n))
    (define (yk k)
        f (+ a (* h k)))
    (define (simpson-term k)
        (* (cond (or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2))
            (yk k))
    (* (/ h 3) (sum simpson-term 0 inc n))
)

(define (sum term a next b)
    (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))
))

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a) (+ result (term a)))
    ))
    (iter a 0))

(define (product term a next b)
    (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (product term a next b)
    (define (iter a results)
        (if (> a b)
        result)
        (iter (next a) (* result (term a))))
    (iter a 1))


(define (accumulate combiner null-value term a next b)
    (if (a > b) (null-value)

    (combiner (term a) (accumulate combiner null-value (next a) next b)))
    )

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
        result
        )
        (iter (next a) (combiner result (term a)))
        )
        (iter a null-value))

(define (filtered-accumulate combiner null-value term a next b filter)
    (if (a > b) (null-value)
    (if (filter a)
        (combiner (term a) (filtered-accumulate combiner null-value (next a) next b))))
        (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)
    ))

(define (filtered-accumulate combiner null-value term a next b filter)
    (define (iter a result)
        (cond ((> a b) result)
        ((filter a) (iter (next a) (combiner result (term a)))
        (else (iter (next a) (result)))
        )
        (iter a null-value))
))

; use lambda to specify anonymous procedure for binding local variables
; body of f then becomes a single call to that procedure

(define (f x y)
    ((lambda (a b)
     (+ (* x (square a))
        (* y b)
     (* a b)
     )
    ))
(+ 1 (* x y))
(- 1 y))

(define (f x y)
    (let (a ( + 1 (* x y)))
        (b (- 1 y))
    )
    (+ (* x (square a))
    (* y b)
    (* a b))
    )

; let var1 have the value exp1 and var2 have value exp2 in body

(define (fixed-point f first-guess)

    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
        tolerance)
        )
    (define (try guess) 
        (let ((next (f guess)))
          (if (close-enough? guess next)
          next
          (try next))  
        )
        )(try first-guess)
        )

; average damping

(define (average-damp f)
    (lambda (x) 
        (average x (f x))))

(define (sqrt x)
    (fixed-point
        (average-damp
            (lambda (y) (/ x y))
        ) 1.0
    ))

(define (deriv g)
    (lambda (x)
     (/ (- g (+ x dx)) (g x)) dx)
    )

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x)
    (- x (/ (g x)
    ((deriv g) x)))))


(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define (sqrt x)
    (newtons-method
    (lambda (y)
    (- (square y) x))
    1.0))

(define (cubic a b c)
    (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)
    ))

(define (double f)
    (lambda x)
    (f x)
)

(define (compose f g)
    (lambda (x))
    (f (g x))
    )

(define x (cons 1 2))

(car x) ; head
1

(cdr x) ; tail
2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z)) ; 1
(car (cdr z)) ; 3


(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n d)
        (/ d g))  
    ))


(define (cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
            ((= m 1) y)
            
            (else
            (error "Argument not 0 or 1: CONS" m)))
        
        ) dispatch)

(define (car z)
    (z 0))

(define (cdr y)
    (z 1))

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (car x)
    (define (car-iter x count)
    (if (= 0 (remainer x 2))
        (car-iter (/ x 2) (+ 1 count))
        count
    ))
    (car-iter x 0))

(define (cdr x)
    (define (cdr-iter x count)
        (if (= 0 (remainder 3))
        (cdr-iter (/ x 3) (+ 1 count))
        count
        ))
        (cdr-iter x 0))


(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f((n f) x)))))

; church numerals

(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))

(lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x) f) x))))
(lambda (f) (lambda (x) (f (f x))))

(define (one (lambda (f) (lambda (x) (f (f x))))))

(add-1 one) ;two

(cons 1 (cons 2 (cons 3 (cons 4 nil)))) ; list
; above is the same as 
(list 1 2 3 4)

; list operations
(define (list-ref items n)
    (if (= n 0)
    (car items)
    (list-ref (cdr items)
    (- n 1)))
)

; null checks if list is empty

((define (length items)
    (if (null? items)
    0
    (+ 1 (length (cdr items))))))

(define (length items)
    ((define (length-iter a count)
        (if (null? a)
        count
        (
            (length-iter (cdr a) (+ count 1))
        ))))
        length-iter items 0)   

; append 

(define (append list1 list2)
    (if (null? list1)
    list2
    (cons (car list1)
    (append (cdr list1) list2
    )
    )))

(define (last-pair list)
    (let ((rest (cdr items))))
    (if (null? rest)
        items
        (last-pair rest))    
    )

(define (reverse list)
    (define (iter items results)
        (if (null? items)
        results
        (iter (cdr results) (cons (car items) result))
        
        ))
        (iter items nil))

(define (scale-list items factor)
    (if (null? items)
    nil
    (cons (* (car items) factor)
    (scale-list (cdr items) factor)
    )
    ))

(define (map proc items)
    (if (null? items)
    nil
    (cons (proc (car items))
    (map proc (cdr items))
    )
    )) 

(define (for-each items proc)
    (cond ((not (null? items))
        (proc (car items))
        (for-each proc (cdr items))
    ))
    )

;recursion is natural tool for dealing with tree structures

(define (count-leaves list)
    (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ count-leaves (car x))
        (count-leaves (cdr x))
        )
    )

)

(define (deep-reverse x)
    (if (pair? x)
        (append (deep-reverse (cdr x))
            (list (deep-reverse (car x)))
        )
    x))

; mobile consists of two branches left and right
(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
   (car (cdr branch)))

; (define (total-weight mobile)
;     (define (iter mobile result)
;         (if (null? (right-branch mobile))
;             result
;             (iter (left-branch mobile) (+ result (branch-length right-branch)))
;         ))
;     (iter mobile 0)
; )

(define (total-weight m)
    (if (not (pair? m))
    m
    (+ (branch-structure (left-branch m))
    (branch-structure (right-branch m)))
    ))

(define (total-weight mobile)
    (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                (total-weight (branch-structure (right-branch mobile)))
        ))
    ))

(define (torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))

(define (balanced? mobile)
    (if (not (pair? mobile))
    true
    (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
        (balanced? (branch-structure (left-branch mobile)))
        (balanced? (branch-structure (right-branch mobile)))
    )
    ))
; map over trees


(define (filter predicate sequence)
    (cond ((null? sequence) nil)
        ((predicate (car sequence))
        (cons (car sequence)
            (filter predicate
                (cdr sequence)
            )
            )
        )
    (else (filter predicate
        (cdr sequence)))
    
    ))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial

        (op (car sequence)
            (accumulate op
                initial
                (cdr sequence)
            )
        )
    ))

(define (enumerate-interval low high)
    (if (> low high)
    nil
    (cons low 
        (enumerate-interval
        (+ low 1)
        high
        )
    )
    ))

; solve the queens board problem

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions)
                    (safe? k positions)
                )
                (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                        (adjoin-positions
                        new-row)
                        ))
                    )
                )
            )

        
        )))