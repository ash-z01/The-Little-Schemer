;; 我的天！都是星星
(import (rnrs))



(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

; 'a  '(a b (a c) ((a b)) ((a b) a c))  => '(b (c) ((b)) ((b) c))
(define rember*
    (lambda (a l)
        (cond 
            ((null? l) '())
            ((atom? (car l))
                (cond 
                    ((eq? a (car l)) (rember* a (cdr l)))
                    (else (cons (car l) (rember* a (cdr l))))))
            (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(display (rember* 'x '(a x b (x c) ((x d (x e (x f ((x g))))) x))))
(newline)

(define insertR*
    (lambda (new old l)
        (cond 
            ((null? l) '())
            ((atom? (car l))
                (cond 
                    ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
                    (else (cons (car l) (insertR* new old (cdr l))))))
            (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(display (insertR* 'x 'b '(a b b (b c) ((b d (b e (b f ((b g))))) b))))
(newline)

(define insertL*
    (lambda (new old l)
        (cond 
            ((null? l) '())
            ((atom? (car l))
                (cond 
                    ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                    (else (cons (car l) (insertL* new old (cdr l))))))
            (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(display (insertL* 'x 'b '(a b b (b c) ((b d (b e (b f ((b g))))) b))))
(newline)


(define occur*
    (lambda (a l)
        (cond 
            ((null? l) 0)
            ((atom? (car l))
                (cond 
                    ((eq? a (car l)) (+ 1 (occur* a (cdr l))))
                    (else (occur* a (cdr l)))))
            (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(display (occur* 'b '(a b b (b c) ((b d (b e (b f ((b g))))) b))))
(newline)

(define replace*
    (lambda (new old l)
        (cond 
            ((null? l) '())
            ((atom? (car l))
                (cond 
                    ((eq? old (car l)) (cons new (replace* new old (cdr l))))
                    (else (cons (car l) (replace* new old (cdr l))))))
            (else (cons (replace* new old (car l)) (replace* new old (cdr l)))))))

(display (replace* 'x 'b '(a b b (b c) ((b d (b e (b f ((b g))))) b))))
(newline)

(define member*
    (lambda (a l)
        (cond 
            ((null? l) #f)
            ((atom? (car l))
                (cond 
                    ((eq? a (car l)) #t)
                    (else (member* a (cdr l)))))
            (else (or (member* a (car l)) (member* a (cdr l)))))))
(display (member* 'e '(a b (a (a (a b) (a (e)))))))
(newline)


(define leftmost
    (lambda (l)
        (cond 
            ((atom? (car l)) (car l))
            (else (leftmost (car l))))))
(display (leftmost '(((a) e) b c)))
(newline)

(define eqlist?
    (lambda (l1 l2)
        (cond 
            ((null? l1)
                (cond 
                    ((null? l2) #t)
                    (else #f)))
            ((null? l2)
                (cond 
                    ((null? l1) #t)
                    (else #f)))
            ; ((and (null? l1) (null? l2)) #t)
            ; ((or (null? l1) (null? l2)) #f)
            ((atom? (car l1))
                (cond 
                    ((atom? (car l2)) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                    (else #f)))
            ((atom? (car l2))
                (cond 
                    ((atom? (car l1)) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
                    (else #f)))
            (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)) )))))
(display (eqlist? '((e a c (e e (e (e z)))) a b c (a c)) '((e a c (e e (e (e z)))) a b c (a c))))
(display (eqlist? '(a) '()))
(newline)


