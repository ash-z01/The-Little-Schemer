;; 数字游戏
(import (rnrs))

(define add1
    (lambda (n)
        (+ n 1)))

(define sub1
    (lambda (n)
        (- n 1)))

(display (add1 6))
(display (sub1 5))
(display (sub1 0))

(display (zero? 0))
(display (zero? 1119))

(define o+
    (lambda (n m)
        (cond 
            ((zero? m) n)
            (else (add1 (o+ n (sub1 m)))))))

(define o-
    (lambda (n m)
        (cond 
            ((zero? m) n)
            (else (sub1 (o- n (sub1 m)))))))

(display (o+ 6 3))
(display (o- 5 2))


(define addtup
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (o+ (car l) (addtup (cdr l)))))))
(display (addtup '(1 2 3 4 5)))

(define x
    (lambda (n m)
        (cond 
            ((zero? m) 0)
            (else (o+ n (x n (sub1 m)))))))

(display (x 5 5))


(define tup+
    (lambda (tup1 tup2)
        (cond 
            ; ((and (null? tup1) (null? tup2)) '())
            ((null? tup1) tup2)
            ((null? tup2) tup1)
            (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(display (tup+ '(1 2 3 4) '(5 6 7 8 9)))


(define gt?
    (lambda (x y)
        (cond 
            ((zero? x) #f)
            ((zero? y) #t)
            (else (gt? (sub1 x) (sub1 y))))))
(define lt?
    (lambda (x y)
        (cond 
            ((zero? y) #f)
            ((zero? x) #t)
            (else (lt? (sub1 x) (sub1 y))))))

(define ==
    (lambda (x y)
        (cond 
            ((zero? y) (zero? x))
            ((zero? x) #f)
            (else (== (sub1 x) (sub1 y))))))

(display (gt? 3 2))
(display (gt? 3 3))
(display (lt? 1 3))
(display (lt? 3 3))
(display (== 1 3))
(display (== 3 3))
(newline)

(define pow
    (lambda (a b)
        (cond 
            ((zero? b) 1)
            (else (x a (pow a (sub1 b)))))))

(display (pow 1 1))
(display (pow 2 3))
(display (expt 2 10))


(define quotient
    (lambda (n m)
        (cond 
            ((< n m) 0)
            (else (add1 (quotient (o- n m) m))))))
(newline)
(display (quotient 8 2))
(newline)
(display (/ 5 13))
(display (div 5 13))
(display (div 5 13.0))
(newline)


(define len
    (lambda (lat)
        (cond 
            ((null? lat) 0)
            (else (+ 1 (len (cdr lat)))))))
(display (len '(1 2 3 (4 5))))
(display (length '(1 2 3 4 (1 2))))
(newline)

(define pick
    (lambda (n lat)
        (cond 
            ((zero? (sub1 n)) (car lat))
            (else (pick (sub1 n) (cdr lat))))))
(display (pick 3 '(1 2 3 4 5)))
(newline)

(define rempick
    (lambda (n lat)
        (cond 
            ((zero? (sub1 n)) (cdr lat))
            (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(display (rempick 3 '(a b c d)))
(newline)

(display (number? 1))
(define rm-nums
    (lambda (lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((number? (car lat)) (rm-nums (cdr lat)))
                    (else (cons (car lat) (rm-nums (cdr lat)))))))))
(display (rm-nums '(a 1 b 2 c 3 d 4 e)))
(newline)

(define get_all_nums
    (lambda (lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((number? (car lat)) (cons (car lat) (get_all_nums (cdr lat))))
                    (else (get_all_nums (cdr lat))))))))

(display (get_all_nums '(a 1 b 2 c 3 d 4 e)))
(newline)

(define eqan
    (lambda (a1 a2)
        (cond 
            ((and (number? a1) (number? a2)) (= a1 a2))
            ((or (number? a1) (number? a2)) #f)
            (else (eq? a1 a2)))))
(display (eqan 1  2))
(display (eqan 'a1 'a1))
(newline)

(define occur
    (lambda (a lat)
        (cond 
            ((null? lat) 0)
            (else (cond 
                    ((eq? a (car lat)) (+ 1 (occur a (cdr lat))))
                    (else (occur a (cdr lat))))))))
(display (occur 'a '(s a b c a d a e a)))
(newline)

(define one?
    (lambda (n)
        (= n 1)))
(display (one? 1))

(define new_rempick
    (lambda (n lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((one? n) (cdr lat))
                    (else (cons (car lat) (new_rempick (- n 1) (cdr lat)))))))))
(display (new_rempick 3 '(a b c d)))