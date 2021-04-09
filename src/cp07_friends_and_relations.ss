;; 朋友及关系
(import (rnrs))


(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
(define member?
    (lambda (a lat)
        (cond 
            [(null? lat) #f]
            [else (or (eq? a (car lat)) (member? a (cdr lat)))])))

(define set?
    (lambda (l)
        (cond 
            ((null? l) #t)
            (else (cond 
                    ((member? (car l) (cdr l)) #f)
                    (else (set? (cdr l))))))))
(display (set? '(a b c c)))
(newline)

(define makeset
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cond 
                    ((member? (car l) (cdr l)) (makeset (cdr l)))
                    (else (cons (car l) (makeset (cdr l)))))))))
(display (makeset '(a b c a c b c)))
(newline)

(define multirember
    (lambda (a l)
        (cond 
            ((null? l) '())
            (else (cond 
                ((eq? a (car l)) (multirember a (cdr l)))
                (else (cons (car l) (multirember a (cdr l)))))))))
(define makeset2
    (lambda (l)
        (cond 
            ((null? l) '())
            (else (cons (car l) (makeset2 (multirember (car l) l)))))))
(display (makeset2 '(a a b a c a c b c)))
(newline)

(define subset?
    (lambda (set1 set2)
        (cond 
            ((null? set1) #t)
            ((member? (car set1) set2) (subset? (cdr set1) set2))
            (else #f))))
(display (subset? '(a c b c d) '(a b c)))
(newline)

(define eqset?
    (lambda (set1 set2)
        (and (subset? set1 set2) (subset? set2 set1))))
(display (eqset? '(a b c e) '(a e c b)))
(newline)

(define intersect?
    (lambda (set1 set2)
        (cond 
            ((null? set1) #f)
            ((cond 
                ((member? (car set1) set2) #t)
                (else (intersect? (cdr set1) set2))))
            (else #f))))
(display (intersect? '(a c) '(d b c)))
(newline)

(define intersect
    (lambda (set1 set2)
        (cond 
            ((null? set1) '())
            ((cond 
                ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
                (else (intersect (cdr set1) set2))))
            (else '()))))
(display (intersect '(a c d) '(d b c d)))
(newline)

(define union
    (lambda (set1 set2)
        (cond 
            ((null? set1) set2)
            ((member? (car set1) set2) (union (cdr set1) set2))
            (else (cons (car set1) (union (cdr set1) set2))))))
(display (union '(a b c) '(b c d e)))
(newline)

(define intersectall
    (lambda (lset)
        (cond 
            ((null? (cdr lset)) (car lset))
            (else (intersect (car lset) (intersectall (cdr lset)))))))
(display (intersectall '((a b e) (a c e) (b c d a e))))
(newline)

(define a_pair?
    (lambda (k)
        (cond 
            ((atom? k) #f)
            ((null? k) #f)
            ((null? (cdr k)) #f)
            ((null? (cdr (cdr k))) #t)
            (else #f))))
(display (a_pair? '(x x)))
(display (a_pair? '(x x x)))
(newline)

;; ((a 8) (b 3)) => ((8 a) (3 b))
(define revrel
    (lambda (rel)
        (cond 
            ((null? rel) '())
            (else (cons (cons (car (cdr (car rel))) (car (car rel))) (revrel (cdr rel)))))))
(display (revrel '((a 1) (b 2) (c 3))))
(newline)


; (define cookie
;     (lambda ()
;         (bake
;             '(350 degrees)
;             '(12 minutes)
;             (mix 
;                 '(walnuts 1 cup)
;                 '(chocolate-chips 16 ounces)
;                 )
;                 (mix 
;                     (mix 
;                         '(flour 2 cups)
;                         '(oatmeal 2 cups)
;                         '(salt .5 teaspoon)
;                         '(baking-powder 1 teaspoon)
;                         '(baking-soda 1 teaspoon)
;                         )
;                     (mix 
;                         '(eggs 2 large)
;                         '(vanilla 1 teaspoon)
;                         (cream 
;                             '(butter 1 cup)
;                             '(sugar 2 cups)
;                             )
;                         )))))



