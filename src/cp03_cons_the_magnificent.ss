;; 用 cons 构筑恢宏
(import (rnrs))

(define rember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? a (car lat)) (cdr lat))
                    (else (cons (car lat) (rember a (cdr lat)))))))))

(define rember_up1
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            ((eq? a (car lat)) (cdr lat))
            (else (cons (car lat) (rember_up1 a (cdr lat)))))))

(display (rember 'and '(bacon lettuce and tomato)))
(display (rember_up1 'sauce '(soy sauce and tomato sauce)))

(define firsts
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car (car l)) (firsts (cdr l)))))))

(display (firsts '((bacon lettuce and tomato))))
(display (firsts '((bacon lettuce and tomato) (haha heihei huhu) (boom sha ka la ka))))

(define insertR
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons old (cons new (cdr lat))))
                    (else (cons (car lat) (insertR new old (cdr lat)))))))))

(define insertL
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons new lat))
                    (else (cons (car lat) (insertL new old (cdr lat)))))))))

(display (insertR 'e 'c '(a b c d)))
(display (insertL 'e 'c '(a b c d)))


(define subst
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons new (cdr lat)))
                    (else (cons (car lat) (subst new old (cdr lat)))))))))

(display (subst 'e 'c '(a b c d)))

(define subst2
    (lambda (new old1 old2 lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((or (eq? old1 (car lat)) (eq? old2 (car lat))) (cons new (cdr lat)))
                    (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))))

(display (subst2 'e 'c 'a '(a b c d)))

(newline)

(define multirember
    (lambda (a lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                ((eq? a (car lat)) (multirember a (cdr lat)))
                (else (cons (car lat) (multirember a (cdr lat)))))))))

(display (multirember 'a '(a a b c a d a e a f)))

(define mulitiinsertR
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons old (cons new (mulitiinsertR new old (cdr lat)))))
                    (else (cons (car lat) (mulitiinsertR new old (cdr lat)))))))))

(display (mulitiinsertR 'x 'c '(a b c d c e c f)))

(define mulitiinsertL
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons new (cons old (mulitiinsertL new old (cdr lat)))))
                    (else (cons (car lat) (mulitiinsertL new old (cdr lat)))))))))

(display (mulitiinsertL 'x 'c '(a b c d c e c f)))

(define multisubst
    (lambda (new old lat)
        (cond 
            ((null? lat) '())
            (else (cond 
                    ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
                    (else (cons (car lat) (multisubst new old (cdr lat)))))))))

(display (multisubst 'x 'd '(a d b d c d e d f d g)))