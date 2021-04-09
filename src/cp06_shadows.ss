;; 如影随行
(import (rnrs))

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define numbered? 
    (lambda (aexp)
        (cond 
            ((atom? aexp)(number? aexp))
            (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
(display (numbered? '(1 + 1) ))
(newline)

(define value
    (lambda (nexp)
        (cond 
            ((atom? nexp) nexp)
            ((eq? (car (cdr nexp)) (quote +) ) (+ (car nexp) (value (car (cdr (cdr nexp))))))
            ((eq? (car (cdr nexp)) (quote -) ) (- (car nexp) (value (car (cdr (cdr nexp))))))
            ((eq? (car (cdr nexp)) (quote x) ) (* (car nexp) (value (car (cdr (cdr nexp))))))
            (else (/ (car nexp) (value (car (cdr (cdr nexp)))))))))
(display (value '(19 x 12)))
(newline)

(define f1st_sub_exp
    (lambda (aexp)
        (car (cdr aexp))))
(display (f1st_sub_exp '(a - 5)))
(newline)
(define f2st_sub_exp
    (lambda (aexp)
        (car (cdr (cdr aexp)))))
(display (f2st_sub_exp '(a - 2 + 1)))
(newline)
(define operate
    (lambda (aexp)
        (car aexp)))