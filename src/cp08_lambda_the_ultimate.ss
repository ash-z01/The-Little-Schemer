;; Lambda 终结者
(import (rnrs))

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
(define rember_f
    (lambda (test?)
        (lambda (a l)
            (cond 
                ((null? l) '())
                ((test? a (car l)) (cdr l))
                (else (cons (car l) ((rember_f test?) a (cdr l))))))))
(display ((rember_f eq?) 'a '(e a b c d)))
(newline)

(define insertL_f
    (lambda (test?)
        (lambda (new old l)
            (cond 
                ((null? l) '())
                ((test? old (car l)) (cons new (cons old (cdr l))))
                (else (cons (car l) ((insertL_f test?) new old (cdr l))))))))
(display ((insertL_f eq?) 'e 'a '(c a b d)))
(newline)

(define insertR_f
    (lambda (test?)
        (lambda (new old l)
            (cond 
                ((null? l) '())
                ((test? old (car l)) (cons old (cons new (cdr l))))
                (else (cons (car l) ((insertR_f test?) new old (cdr l))))))))
(display ((insertR_f eq?) 'e 'a '(c a b d)))
(newline)

(define insertG_f
    (lambda (test?)
        (lambda (new old l)
            (cond 
                ((null? l) '())
                ((test? old (car l)) (cons new (cons old (cons new (cdr l)))))
                (else (cons (car l) ((insertG_f test?) new old (cdr l))))))))
(display ((insertG_f eq?) 'e 'a '(c a b d)))
(newline)

(define multirember&co
    (lambda (a lat col)
        (cond 
            ((null? lat) (col '() '()))
            ((eq? a (car lat)) 
                (multirember&co a (cdr lat)
                    (lambda (newlat seen)
                        (col newlat (cons (car lat) seen)))))
            (else 
                (multirember&co a (cdr lat)
                    (lambda (newlat seen)
                        (col (cons (car lat) newlat) seen)))))))

(display (multirember&co 'even '() (lambda (x y) (null? y))))
(newline)
(display (multirember&co 'even '(even) (lambda (x y) (null? y))))
(newline)
(display (multirember&co 'even '(ash even) (lambda (x y) (null? y))))
(newline)


(define multiinsertLR
    (lambda (new oldL oldR lat)
        (cond 
            ((null? lat) '())
            ((eq? oldL (car lat)) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
            ((eq? oldR (car lat)) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
            (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))
(display (multiinsertLR 'salty 'fish 'chips '(chips and fish or fish and chips)))
(newline)


(define multiinsertLR&co
    (lambda (new oldL oldR lat col)
        (cond 
            ((null? lat) (col '() 0 0))
            ((eq? oldL (car lat)) 
                (multiinsertLR&co new oldL oldR (cdr lat) 
                    (lambda (newlat L R) (col (cons new (cons oldL newlat)) (+ L 1) R))))
            ((eq? oldR (car lat)) 
                (multiinsertLR&co new oldL oldR (cdr lat) 
                    (lambda (newlat L R) (col (cons oldR (cons new newlat)) L (+ R 1)))))
            (else 
                (multiinsertLR&co new oldL oldR (cdr lat) 
                    (lambda (newlat L R) (col (cons (car lat) newlat) L R)))))))

(display (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (newlat L R) newlat)))(newline)
(display (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (newlat L R) L)))(newline)
(display (multiinsertLR&co 'salty 'fish 'chips '(chips and fish or fish and chips) (lambda (newlat L R) R)))(newline)


(define my_even?
    (lambda (n)
        (= (* (div n 2) 2) n)))

(define even_only*
    (lambda (l)
        (cond 
            ((null? l) '())
            ((atom? (car l))
                (cond 
                    ((even? (car l)) (even_only* (cdr l)))
                    (else (cons (car l) (even_only* (cdr l))))))
            (else (cons (even_only* (car l)) (even_only* (cdr l)))))))
(display (even_only* '(2 3 5 4 (2 4 5) (4 2 (3 4 (4) (2 (3)))))))(newline)

(define even_only*&co
    (lambda (l col)
        (cond 
            ((null? l) (col '() 1 0))
            ((atom? (car l))
                (cond 
                    ((even? (car l)) 
                        (even_only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* p (car l)) s))))
                    (else 
                        (even_only*&co (cdr l) (lambda (newl p s) (col newl p (+ s (car l))))))))
            (else 
                (even_only*&co (car l) 
                    (lambda (al ap as) 
                        (even_only*&co (cdr l)
                            (lambda (dl dp ds)
                                (col (cons al dl) (* ap dp) (+ as ds))))))))))
(display (even_only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (newl product sum) (cons sum (cons product newl)))))
(newline)
