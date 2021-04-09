;; 处理，处理，反复处理...
(import (rnrs))
; (#%$assembly-output #t) ;输出汇编代码

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define lat? 
    (lambda (l)
        (cond
            ((null? l) #t)
            ((atom? (car l)) (lat? (cdr l)))
            (else #f))))

(display (lat? '(Jack Sprat could eat no chicken fat)))
(display (or (null? '()) (null? '(d e f g))))

(define my_member?
    (lambda (a lat)
        (cond 
            [(null? lat) #f]
            [(or (eq? a (car lat)) (my_member? a (cdr lat))) #t]
            [else #f])))

(define member?
    (lambda (a lat)
        (cond 
            [(null? lat) #f]
            [else (or (eq? a (car lat)) (member? a (cdr lat)))])))


(display (member? 'tea '(coffee tea milk)))
