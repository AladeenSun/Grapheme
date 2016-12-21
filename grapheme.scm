; Some fundamental operations for Grapheme

; Functional attributes

(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)  (lambda (arg) (func arg1 arg)))
(define (compose f g)      (lambda (arg) (f (g arg))))


; Basic functions
(define (not x)            (if x #f #t))
(define (id obj)           obj)
(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define epsilon?           (curry eq? '#))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))
(define (abs x)            ((> x 0) x -x))


'Done!
