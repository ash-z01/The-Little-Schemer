;; ...周而复始...
(import (rnrs))

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))
(define pick
    (lambda (n lat)
        (cond 
            ((zero? (- n 1)) (car lat))
            (else (pick (- n 1) (cdr lat))))))
(define looking
    (lambda (a lat)
        (keep_looking a (pick 1 lat) lat)))

(define keep_looking
    (lambda (a sorn lat)
        (cond 
            ((number? sorn)
                (keep_looking a (pick sorn lat) lat))
            (else 
                (eq? a sorn)))))

(display (looking 'caviar '(6 2 4 caviar 5 7 3)))(newline)
(display (looking 'caviar '(6 2 grits caviar 5 7 3)))(newline)
; (display (looking 'caviar '(7 2 4 7 5 6 3)))(newline)

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))
(define shift
    (lambda (pair)
        (build (first (first pair))
            (build (second (first pair))
                (second pair)))))
(display (shift '((a b) c)))
(display (shift '((a b) (c d))))
(display (shift '((a (b f)) (c (d e)))))
(newline)

(define align
    (lambda (pora)
        (cond 
            ((atom? pora) pora)
            ((a-pair? (first pora))
                (align (shift pora)))
            (else (build (first pora) (align (second pora)))))))
(display (align '((a b) (c (d e)))))
(display (align '((a (b f)) (c (d e)))))
(newline)

(define length*
    (lambda (pora)
        (cond 
            ((atom? pora) 1)
            (else (+ (length* (first pora)) (length* (second pora)))))))
(display (length* '((a (b f)) (c (d e)))))
(newline)

(define weight*
    (lambda (pora)
        (cond 
            ((atom? pora) 1)
            (else 
                (+  (* (weight* (first pora)) 2)
                    (weight* (second pora)))))))
(display (weight* '((a b) c)))(newline)
(display (weight* '(a (b c))))(newline)
(display (weight* '((a b) (c d))))(newline)
(display (weight* '((a (b f)) (c (d e)))))(newline)

(define revpair
  (lambda (p)
    (build (second p) (first p))))
(define shuffle
    (lambda (pora)
        (cond 
            ((atom? pora) pora)
            ((a-pair? (first pora))
                (shuffle (revpair pora)))
            (else (build (first pora) (shuffle (second pora)))))))
(display (shuffle '((a b) c)))(newline)


(define one?
  (lambda (n) (= n 1)))
;; 克拉茨猜想
(define C
    (lambda (n)
        (cond 
            ((one? n) 1)
            (else 
                (cond 
                    ((even? n) (C (/ n 2)))
                    (else (C (+ 1 (* 3 n)))))))))
(display (C 3))(newline)

;; 阿克曼函数
(define A
    (lambda (m n)
        (cond 
            ((zero? m) (+ 1 n))
            ((zero? n) (A (- m 1) 1))
            (else 
                (A (- m 1) (A m (- n 1)))))))
(display (A 1 0))(newline)
(display (A 1 1))(newline)
(display (A 2 2))(newline)
; (display (A 4 1))(newline)
; (display (A 4 3))(newline)


;; 停机问题
(define will_stop?
    (lambda (f)
        (not (null? (f '())))))
(define eternity
    (lambda (x)
        (eternity x)))
(define last_try
    (lambda (x)
        (and (will_stop? last_try) (eternity x))))
; (display (will_stop? length))
; (display (will_stop? eternity)) => #f
; (display (will_stop? last_try))

(display "---------------------------------------------\n")
(display '=============================================)(newline)
;; application-order Y combinator  [应用序 Y 组合子]
(define mylen
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (+ 1 (mylen (cdr l)))))))
; (display (mylen '(1 2 3 4 5)))

;; length0
(display (
(lambda (l)
    (cond 
        ((null? l) 0)
        (else (+ 1 (eternity (cdr l))))))
'()
))

;; length<=1
(display (
(lambda (l)
    (cond 
        ((null? l) 0)
        (else (+ 1 (
            (lambda (l)
                (cond 
                    ((null? l) 0)
                    (else (+ 1 (eternity (cdr l)))))) (cdr l))))))
'(1)
))

;; length<=2
(display (
(lambda (l)
    (cond 
        ((null? l) 0)
        (else (+ 1 (
            (lambda (l)
                (cond 
                    ((null? l) 0)
                    (else (+ 1 (
                        (lambda (l)
                            (cond 
                                ((null? l) 0)
                                (else (+ 1 (eternity (cdr l)))))) (cdr l)))))) (cdr l))))))
'(1 2)
))

;; length0
(display (
((lambda (len)
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (+ 1 (len (cdr l)))))))
eternity)
'()
))

;; length<=1
(display (
((lambda (f)
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (+ 1 (f (cdr l)))))))
  ((lambda (g)
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (+ 1 (g (cdr l)))))))
eternity))
'(1)
))

;; length<=2
(display (
((lambda (len)
    (lambda (l)
        (cond 
            ((null? l) 0)
            (else (+ 1 (len (cdr l)))))))
    ((lambda (len)
        (lambda (l)
          (cond 
            ((null? l) 0)
            (else (+ 1 (len (cdr l)))))))
    ((lambda (len)
        (lambda (l)
          (cond 
            ((null? l) 0)
            (else (+ 1 (len (cdr l)))))))
eternity)))
'(1 2)
))

;; length0  ;;make-len mk-length 
(display (
((lambda (make_len)
    (make_len eternity))
    (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l))))))))
'()
))

;; length<=1
(display (
((lambda (make_len)
    (make_len
        (make_len eternity)))
    (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l))))))))
'(1)
))


;; length<=2
(display (
((lambda (make_len)
    (make_len
        (make_len
            (make_len eternity))))
    (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l))))))))
'(1 2)
))

;; length0
(display (
((lambda (make_len)
    (make_len make_len))
    (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l))))))))
'()
))
(display (
((lambda (make_len)
    (make_len make_len))
    (lambda (make_len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (make_len (cdr l))))))))
'()
))

(newline)

(display (
(   
    (lambda (make_len)
        (make_len make_len))
    (lambda (make_len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 ((make_len make_len) (cdr l)))))))
)
'(1 2 3)
))

(newline)

(display (
(
    (lambda (make_len)
        (make_len make_len))
    (lambda (make_len) (
        (lambda (len)
            (lambda (l)
                (cond 
                    ((null? l) 0)
                    (else (+ 1 (len (cdr l)))))))
        (lambda (x)
            ((make_len make_len) x))))
)
'(1 2 3 4 5)
))


(newline)

(display (
(
    (lambda (le)
        ((lambda (make_len)
            (make_len make_len))
        (lambda (make_len) 
            (le (lambda (x)
                    ((make_len make_len) x))))))
    (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l)))))))
)
'(1 2 3 4 5)
))

(newline)

;; final
(define Y
    (lambda (le)
        ((lambda (f) (f f))
            (lambda (f)
                (le (lambda (x) ((f f) x)))))))


(display ((Y (lambda (len)
        (lambda (l)
            (cond 
                ((null? l) 0)
                (else (+ 1 (len (cdr l)))))))) '(1 2 3)))
