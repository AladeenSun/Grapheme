; List library for Grapheme

(load "grapheme.scm")

; Basic list operations

(define (null? obj)        (if (eqv? obj '()) #t #f))
(define (list . objs)       objs)

(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))

(define (foldr func accum lst)
  (if (null? lst)
      accum
      (func (car lst) (foldr func accum (cdr lst)))))

(define (unfold pred it nxt init act)
  (if (pred it)
      init
      (act it (unfold pred (nxt it) nxt init act))))

(define fold foldl)
(define reduce fold)

(define (length lst)        (fold (lambda (x y) (+ x 1)) 0 lst))
(define (list-ref lst pos)
   (if (< pos 0)
       #f
       (if (eq? pos 0)
           (if (null? lst)
               #f
               (car lst))
           (list-ref (cdr lst (- pos 1))))))


; Syntactic sugar for list-ref

(define (first lst)         (list-ref 0 lst))
(define (second lst)        (list-ref 1 lst))
(define (third lst)         (list-ref 2 lst))
(define (fourth lst)        (list-ref 3 lst))
(define (fifth lst)         (list-ref 4 lst))
(define (sixth lst)         (list-ref 5 lst))
(define (seventh lst)       (list-ref 6 lst))
(define (eighth lst)        (list-ref 7 lst))
(define (ninth lst)         (list-ref 8 lst))
(define (tenth lst)         (list-ref 9 lst))
(define (last lst)          (list-ref (- (length lst) 1) lst))

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))


; List iterations

(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (ormap func . lst) (fold (lambda (x y) (or (func y) x)) #f lst))
(define (andmap func . lst) (fold (lambda (x y) (and (func y) x) #t lst)))
(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (filter-not pred lst) (foldr (lambda (x y) (if (not (pred x)) (cons x y) y)) '() lst))

(define (for-each func lst) 
  (if (null? lst)
      #t
      (func (car lst))) 
  (if (null? lst)
       #t
       (for-each func (cdr lst))))

(define (sum . lst)         (fold + 0 lst))
(define (product . lst)     (fold * 1 lst))
(define (and . lst)         (fold && #t lst))
(define (or . lst)          (fold || #f lst))
(define (any? pred . lst)   (apply or (map pred lst)))
(define (every? pred . lst) (apply and (map pred lst)))
(define (max x . num-list) (fold (lambda (y z) (if (> y z) y z)) x num-list))
(define (min x . num-list) (fold (lambda (y z) (if (< y z) y z)) x num-list))


;Other list functions

(define (append lst . lsts) 
   (define (append2 lstx lsty) (if (null? lstx) lsty (cons (car lstx) (append2 (cdr lstx) lsty)))) 
   (append2 lst (foldr append2 '() lsts)))

(define (reverse lst)       (fold (flip cons) '() lst))

(define (sort lst func)
   (define (insert x lst) 
      (if (null? lst) (list x) (if (func x (car lst)) (cons x lst) (cons (car lst) (insert x (cdr lst))))))
   (foldl (lambda (x y) (insert y x)) '() lst))

(define (remove obj lst . func)
   (define f 
  (if (null? func)
       eq?
            (car func)))
   (define (rem obj lst fc)
        (if (null? lst)
            '()
            (if (fc obj (car lst))
                (cdr lst)
                (cons (car lst) (rem obj (cdr lst) fc)))))
   (rem obj lst f))

(define (remq obj lst) (remove obj lst))

(define (remove* obj lst . func)
  (define f
       (if (null? func)
            eq?
           (car func)))
  (define (rem obj lst fc)
       (if (null? lst)
           '()
           (if (fc obj (car lst))
               (rem obj (cdr lst) fc)
               (cons (car lst) (rem obj (cdr lst) fc)))))
  (rem obj lst f))

(define (remq* obj lst) (remove* obj lst))


(define (mem lst func)
   (define res (foldl 
                 (lambda (x y) (if (null? x) 
                                   (if (func y) 
                                       (list y) 
                                        x)
                                    (append x (list y)))) '() lst))
   (if (null? res) #f res))
(define (member v lst . func)
   (define f (if (null? func) (curry eq? v) (car func)))
   (mem lst f))
(define (memv v lst) (mem lst (curry eq? v)))
(define (memq v lst) (mem lst (curry eq? v)))
(define (memf func lst) (mem lst func))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (findq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (findv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (findf func lst)      (fold (mem-helper func id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assf func alist)    (fold (mem-helper func car) #f alist))

(define (flatten lsts)
  (if (not (list? lsts))
      (list lsts)
      (if (null? lsts)
          lsts
          (append (flatten (car lsts)) (flatten (cdr lsts))))))

(define (remove-duplicates lst . func)
  (define f (if (null? func) eq? (car func)))
  (foldr (lambda (x y) (cons x (filter-not (curry f x) y))) '() lst))

(define (count obj lst . func)
   (define f
        (if (null? func)
             eq?
            (car func)))
   (fold (lambda (x y) (if (f obj y) (+ x 1) x)) 0 lst))

(define (range start end . step)
  (define s
       (if (null? step) 1 (car step)))
  (define (build now)
       (if (or (and (> s 0) (> now end))
               (and (< s 0) (< now end)))
           '()
           (cons now (build (+ now s)))))
  (build start))

(define (make-list num v)
  (unfold (curry eq? num) 0 (lambda (x) (+ x 1)) '() (lambda (x y) (cons v y))))

(define (take num lst)      (if (= num 0) '() (cons (car lst) (take (- num 1) (cdr lst)))))
(define (drop num lst)      (reverse (take num (reverse lst))))


(define (zip lst . lsts) 
  (define (append-lists-list lsts lst) (if (null? lst) '() (cons (append (car lsts) (list (car lst))) (append-lists-list (cdr lsts) (cdr lst)))))
  (foldl append-lists-list (map list lst) lsts))

(define (splitf lst func)
	(define (proc lst pred)
		(if (null? lst) (list lst '() '())
						(if (func (car lst)) (list pred (list (car lst)) (cdr lst)) (proc (cdr lst) (append pred (list (car lst)))))))
	(proc lst '()))

(define (split id lst)
	(splitf lst (curry eq? id)))

(define (add-between lst v) (cdr (foldr (lambda (x y) (cons v (cons x y))) '() lst)))

(define (atom-union arg1 arg2)
  (listtoatom (remove-duplicates (append (atomtolist arg1) (atomtolist arg2)))))

