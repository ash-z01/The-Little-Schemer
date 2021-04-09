;; 值是什么
(import (rnrs))

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
      name
      (first entry)
      (second entry)
      entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
        (lookup-in-entry-help
          name
          (cdr names)
          (cdr values)
          entry-f)))))

(display (lookup-in-entry 'sex '((name age sex) (even 8 female)) (lambda (v) v)))
(newline)



(define lookup_in_table 
    (lambda (name table table-f)
        (cond 
            ((null? table) (table-f name))
            (else 
                (lookup-in-entry name (car table) 
                    (lambda (name)
                        (lookup_in_table name (cdr table) table-f)))))))
(display (lookup_in_table 'age '(((name age sex) (even 8 female)) ((name age sex) (ash 88 male))) (lambda (v) v)))
(newline)

(display 
(cons 'car
    (cons 
        (cons 'quote
            (cons 
                (cons 'a
                    (cons 'b
                        (cons 'c
                            (quote ())
                        )
                    )
                )
                (quote ())
            )
        )
        (quote ())
    )
)
)

(display 
    (cons 
        (cons 'quote
            (cons 
                (cons 'c
                    (quote ())
                )
                (quote ())
            )
        )
        (quote ())
    )
)

(display (quote nothing))

(display (car (quote (a b c))))
(display (quote (car (quote (a b c)))))
(display (+ 1 6))
(display 6)
(display (quote nothing))
(newline)

(display  (
    (lambda (nothing)
        (cons nothing (quote ())))
    (quote (from nothing comes something))
))
(display  (
    (lambda (nothing)
        (cond 
            (nothing (quote something))
            (else (quote nothing))))
    #t
))
(newline)
(display "==========================\n")
(define add1
    (lambda (n)
        (+ n 1)))
(define sub1
    (lambda (n)
        (- n 1)))

(define *const
    (lambda (e table)
        (cond 
            ((number? e) e)
            ((eq? e #t) #t)
            ((eq? e #f) #f)
            (else (build (quote primitive) e)))))
; (display (*const #f '()))(newline)

(define text-of second)
(define *quote
    (lambda (e table)
        (text-of e)))
; (display (*quote '(quote (+ 1 2)) '()))(newline)

(define initial_table
    (lambda (name)
        (car (quote name))))
(define *identifier
    (lambda (e table)
        (lookup_in_table e table initial_table)))
; (display (*identifier 'a '(((a b) (1 2)))))(newline)

(define *lambda
    (lambda (e table)
        (build (quote non-primitive)
            (cons table (cdr e)))))
; (display (*lambda '(lambda (x) (+ 1 x)) '(((a b)(1 2)))))(newline)


(define else?
    (lambda (x)
        (cond 
            ((atom? x) (eq? x (quote else)))
            (else #f))))
(define question_of first)
(define answer_of second)
(define cond_lines_of cdr)
(define evcon
    (lambda (lines table)
        (cond
            ((else? (question_of (car lines))) 
                (meaning (answer_of (car lines))
                    table))
            ((meaning (question_of (car lines))
                    table)
            (meaning (answer_of (car lines))
                    table))
            (else (evcon (cdr lines) table)))))
(define *cond
    (lambda (e table)
        (evcon (cond_lines_of e) table)))
; (display (*cond '(cond ((eq? 1 2) (add1 2))(else #f)) '(((a b)(1 2))) ))(newline)

(define extend-table cons)
(define table_of first)
(define formals_of second)
(define body_of third)
(define apply_closure
    (lambda (closure vals)
        (meaning (body_of closure)
            (extend-table 
                (new-entry
                    (formals_of closure)
                    vals)
                    (table_of closure)))))
(define evlis
    (lambda (args table)
        (cond 
            ((null? args) (quote ()))
            (else 
                (cons (meaning (car args) table)
                    (evlis (cdr args) table))))))
(define primitive?
    (lambda (l)
        (eq? (first l) (quote primitive))))
(define non-primitive?
    (lambda (l)
        (eq? (first l) (quote non-primitive))))
(define :atom?
    (lambda (x)
        (cond 
            ((atom? x) #t)
            ((null? x) #f)
            ((eq? (car x) (quote primitive)) #t)
            ((eq? (car x) (quote non-primitive)) #t)
            (else #f))))
(define apply_primitive
    (lambda (name vals)
        (cond 
            ((eq? name (quote cons)) (cons (first vals) (second vals)))
            ((eq? name (quote car)) (car (first vals)))
            ((eq? name (quote cdr)) (cdr (first vals)))
            ((eq? name (quote null?)) (null? (first vals)))
            ((eq? name (quote eq?)) (eq? (first vals) (second vals)))
            ((eq? name (quote atom?)) (:atom? (first vals)))
            ((eq? name (quote zero?)) (zero? (first vals)))
            ((eq? name (quote add1)) (add1 (first vals)))
            ((eq? name (quote sub1)) (sub1 (first vals)))
            ((eq? name (quote number?)) (number? (first vals))))))
(define apply_
    (lambda (fun vals)
        (cond 
            ((primitive? fun)
                (apply_primitive (second fun) vals))
            ((non-primitive? fun)
                (apply_closure
                    (second fun) vals)))))

(define function_of car)
(define arguments_of cdr)
(define *application
    (lambda (e table)
        (apply_ 
            (meaning (function_of e) table)
            (evlis (arguments_of e) table))))
; (display (* ))(newline)

(define atom_to_action
    (lambda (e)
        (cond 
            ((number? e) *const )
            ((eq? e #t) *const )
            ((eq? e #f) *const )
            ((eq? e (quote cons)) *const )
            ((eq? e (quote car)) *const )
            ((eq? e (quote cdr)) *const )
            ((eq? e (quote null?)) *const )
            ((eq? e (quote eq?)) *const )
            ((eq? e (quote atom?)) *const )
            ((eq? e (quote zero?)) *const )
            ((eq? e (quote add1)) *const )
            ((eq? e (quote sub1)) *const )
            ((eq? e (quote number?)) *const )
            (else *identifier ))))
; (display (atom_to_action 'cons ))

(define list_to_action
    (lambda (e)
        (cond 
            ((atom? (car e))
                (cond 
                    ((eq? (car e) (quote quote)) *quote )
                    ((eq? (car e) (quote lambda)) *lambda )
                    ((eq? (car e) (quote cond)) *cond )
                    (else *application )))
            (else *application ))))
(define expr_to_action
    (lambda (e)
        (cond 
            ((atom? e) (atom_to_action e))
            (else (list_to_action e)))))

(define meaning
    (lambda (e table)
        ((expr_to_action e) e table)))
(define value
    (lambda (e)
        (meaning e (quote ()))))




; (display (*lambda '(lambda (x) (cons x y)) '(((y z) ((8) 9))) ))
; (display (*cond '(cond (coffee klatsch) (else party)) '(((coffee) (#t)) ((klatsch party) (5 (6))))))
; (display (apply_closure (quote ((((u v w) (1 2 3)) ((x y z) ( 4 5 6))) (x y) (cons z x))) (quote ((a b c) (d e f)))))
; (display (apply_ '(non-primitive (lambda (x) (add1 x))) '(2)))
(display (*cond '(cond ((eq? 1 1) 1)(else 0)) '()))