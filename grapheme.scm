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

(define (not x)            (if x #f #t))
(define (null? obj)        (if (eqv? obj '()) #t #f))
(define (id obj)           obj)
(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)  (lambda (arg) (func arg1 arg)))
(define (compose f g)      (lambda (arg) (f (g arg))))


(define (list-ref lst pos)
   (if (< pos 0)
       #f
       (if (eq? pos 0)
           (if (null? lst)
               #f
               (car lst))
           (list-ref (cdr lst (- pos 1))))))

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

(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define epsilon?           (curry eq? '#))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))
(define (abs x)            ((> x 0) x -x))
(define (max x . num-list) (fold (lambda (y z) (if (> y z) y z)) x num-list))
(define (min x . num-list) (fold (lambda (y z) (if (< y z) y z)) x num-list))
(define (list . objs)       objs)
(define (length lst)        (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst)       (fold (flip cons) '() lst))
(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (findq obj lst)       (fold (mem-helper (curry eq? obj) id) #f lst))
(define (findv obj lst)       (fold (mem-helper (curry eqv? obj) id) #f lst))
(define (findf func lst)      (fold (mem-helper func id) #f lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) #f alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) #f alist))
(define (assf func alist)    (fold (mem-helper func car) #f alist))

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

(define (splitf lst func)
	(define (proc lst pred)
		(if (null? lst) (list lst '() '())
						(if (func (car lst)) (list pred (list (car lst)) (cdr lst)) (proc (cdr lst) (append pred (list (car lst)))))))
	(proc lst '()))

(define (split id lst)
	(splitf lst (curry eq? id)))

(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) '() lst))
(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))
(define (filter-not pred lst) (foldr (lambda (x y) (if (not (pred x)) (cons x y) y)) '() lst))
(define (add-between lst v) (cdr (foldr (lambda (x y) (cons v (cons x y))) '() lst)))
(define (for-each func lst) 
  (if (null? lst)
      #t
      (func (car lst))) 
  (if (null? lst)
       #t
       (for-each func (cdr lst))))
(define (make-list num v)
  (unfold (curry eq? num) 0 (lambda (x) (+ x 1)) '() (lambda (x y) (cons v y))))
(define (range start end . step)
  (define s
       (if (null? step) 1 (car step)))
  (define (build now)
       (if (or (and (> s 0) (> now end))
               (and (< s 0) (< now end)))
           '()
           (cons now (build (+ now s)))))
  (build start))
(define (remove-duplicates lst . func)
  (define f
       (if (null? func) eq? (car func))))

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

(define (count obj lst . func)
   (define f
        (if (null? func)
             eq?
            (car func)))
   (fold (lambda (x y) (if (f obj y) (+ x 1) x)) 0 lst))

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

(define (sum . lst)         (fold + 0 lst))
(define (product . lst)     (fold * 1 lst))
(define (and . lst)         (fold && #t lst))
(define (or . lst)          (fold || #f lst))
(define (any? pred . lst)   (apply or (map pred lst)))
(define (every? pred . lst) (apply and (map pred lst)))

(define (take num lst)      (if (= num 0) '() (cons (car lst) (take (- num 1) (cdr lst)))))
(define (drop num lst)      (reverse (take num (reverse lst))))

(define (append lst . lsts) 
  (define (append2 lstx lsty) (if (null? lstx) lsty (cons (car lstx) (append2 (cdr lstx) lsty)))) 
  (append2 lst (foldr append2 '() lsts)))

(define (zip lst . lsts) 
  (define (append-lists-list lsts lst) (if (null? lst) '() (cons (append (car lsts) (list (car lst))) (append-lists-list (cdr lsts) (cdr lst)))))
  (foldl append-lists-list (map list lst) lsts))
(define (ormap func . lst) (fold (lambda (x y) (or (func y) x)) #f lst))
(define (andmap func . lst) (fold (lambda (x y) (and (func y) x) #t lst)))

(define (flatten lsts)
  (if (not (list? lsts))
      (list lsts)
      (if (null? lsts)
          lsts
          (append (flatten (car lsts)) (flatten (cdr lsts))))))

(define (remove-duplicates lst . func)
  (define f (if (null? func) eq? (car func)))
  (foldr (lambda (x y) (cons x (filter-not (curry f x) y))) '() lst))


(define (vertexes G) (car G))
(define (arcs G) (cadr G))
(define (vertex G pos) (list-ref (vertexes G) pos))
(define (arc G pos) (list-ref (arcs G) pos))


(define UPPER '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
(define (relabel G)
	(define v (vertexes G))
	(define a (arcs G))
	(define v-map (zip v (take (length v) UPPER)))
	(define (f node) (cadr (assf (curry eqs? node) v-map)))
	(define (v-modify node)
			(define node-new (f node))
			(if (acc? node) (sflip node-new) node-new))
	(define (a-modify edge)
			(define a (car edge))
			(define b (cadr edge))
			(list (f a) (f b) (caddr edge)))
	(define v-new (map v-modify v))
	(define a-new (map a-modify a))
	(cons v-new (list a-new)))

(define (lex G) 
	(filter (lambda (x) (not (eq? x '#))) (remove-duplicates (flatten (map (compose atom-split caddr) (arcs G))))))

(define (atom-union arg1 arg2)
  (listtoatom (remove-duplicates (append (atomtolist arg1) (atomtolist arg2)))))

(define (normal-v v) (map (lambda (x) (if (acc? x) (sflip x) x)) v))

(define (NFAtoREG-add G)
   (define acc (map sflip (filter acc? (vertexes G))))
   (define len (length acc))
   (define new-v (append 
                   (list 'st '#ed) 
                   (map (lambda (x) 
                                (if (acc? x) (sflip x) x))
                        (vertexes G))))
   (define l-ed (make-list len 'ed))
   (define l-s (make-list len '#))
   (define a (arcs G))
   (define new-a (append a (zip acc l-ed l-s) (list (list 'st (caddr new-v) '#))))
   (cons new-v (list new-a)))

(define (NFAtoREG-del G id)
   (define v (vertexes G))
   (define a (arcs G))
   (define new-v (filter-not (curry eqs? id) v))
   (define normal-new-v (normal-v new-v))
   (define (arc-query arg1 arg2 la) (foldl (lambda (x y) (if (and (eq? (car y) arg1) (eq? (cadr y) arg2)) (caddr y) x)) #f la))
   (define (arc-modify arg1 arg2 z la) (foldr (lambda (x y) (if (and (eq? (car x) arg1) (eq? (cadr x) arg2)) (cons (list arg1 arg2 z) y) (cons x y))) '() la))
   (define (arc-remove arg1 arg2 la) (foldr (lambda (x y) (if (and (eq? (car x) arg1) (eq? (cadr x) arg2))) y (cons x y)) '() la))
   (define (arc-add arg1 arg2 z la) (append la (list (list arg1 arg2 z))))
   (define (not-empty? x) (not (eq? x #f)))
   (define id-arc (arc-query id id a))
   (define mid
        (if (and (not-empty? id-arc) (not (epsilon? id-arc))) (wraps id-arc) '#))
   (define (arc-new . lst)
      (apply atom-append (map (lambda (x) (if (> (length (atomtolist x)) 1) (wrapp x) x)) lst)))
   (define (renew arg1 arg2 tmpa)
      (define arc1 (arc-query arg1 id tmpa))
      (define arc2 (arc-query id arg2 tmpa))
      (define arc-old (arc-query arg1 arg2 tmpa))
      (if (and (not-empty? arc1) (not-empty? arc2))
             (if (not-empty? arc-old)
                   (arc-modify arg1 arg2 (atom-union arc-old (arc-new arc1 mid arc2)) tmpa)
                   (arc-add arg1 arg2 (arc-new arc1 mid arc2) tmpa))
             tmpa))

   (define mod-a (foldl
               (lambda (x y)
              (foldl (lambda (z w) (renew w y z)) x normal-new-v)) a normal-new-v))
   (define new-a
      (filter-not (lambda (x) (or (eqs? (car x) id) (eqs? (cadr x) id)))
                  mod-a))
   (cons new-v (list new-a)))


(define (NFAtoREG-res G)
   (caddr (arc G 0)))

(define (NFAtoREG G)
   (NFAtoREG-res (foldl (lambda (x y) (NFAtoREG-del x y)) (NFAtoREG-add G) (normal-v (vertexes G)))))

(define (sort lst func)
   (define (insert x lst) 
	(if (null? lst) (list x) (if (func x (car lst)) (cons x lst) (cons (car lst) (insert x (cdr lst))))))
   (foldl (lambda (x y) (insert y x)) '() lst))

(define (NFAtoDFA G)
   (define v (vertexes G))
   (define a (arcs G))
   (define l (lex G))
   ;(print l)
   (define acc (map sflip (filter acc? v)))
   (define new-v (normal-v v))
   (define (contain? v ch)
	;(print "-----")
	;(print v)
	;(print (atomtolist v))
	;(print ch)
 	;(print (memv ch (atomtolist v)))
	;(print "-----")
        (list? (memv ch (atomtolist v))))
   (define (step id ch)
	(foldl (lambda (x y) (if (and (eq? (car y) id) (contain? (caddr y) ch)) (cons (cadr y) x) x)) '() a))
   (define (closure id now tot)
	(define to-add 
		(filter (lambda (x) (eq? (memv x tot) #f)) (step id '#)))
        (define now-new (append now to-add))
	(define tot-new (append tot to-add))
	(if (> (length now-new) 1)
	    (closure (cadr now-new) (cdr now-new) tot-new)
            (sort tot-new atom<?)))
   (define (epsilon-closure T)
          (closure (car T) T T))
   (define (move T ch)
	 (sort (remove-duplicates (foldl (lambda (x y) (append x (step y ch))) '() T)) atom<?))
   (define closureS (epsilon-closure (list (car new-v))))
   ;(print closureS)
   (define (succ T) (foldl (lambda (x y) (if (null? (move T y)) x (cons (list T (epsilon-closure (move T y)) y) x))) '() l))
   (define (process T now tot)
	(define to-add (filter (lambda (x) (eq? (memv x tot) #f)) (map cadr (succ T))))
	;("Process starts")
	;(print T)
	;(print to-add)
	;(print "Process ends")
	(define now-new (append now to-add))
        (define tot-new (append tot to-add))
     	(if (> (length now-new) 1)
     	    (process (cadr now-new) (cdr now-new) tot-new)
            tot-new))
   (define (convertV T) 
   		;(print T) 
   		(listtoatom T))
   (define (convertVacc T) 
	(define acc-status (fold (lambda (x y) (or x (list? (memq y acc)))) #f T))
	(if acc-status (atom-add '# (convertV T)) (convertV T)))
   (define compound-v (process closureS (list closureS) (list closureS)))
   (define v-map (map (lambda (x) (list x (convertV x))) compound-v))
   (define compound-a (foldl (lambda (x y) (append (succ y) x)) '() compound-v))
   (cons (map convertVacc compound-v)
   	 (list (map (lambda (x) (list (cadr (assq (car x) v-map)) (cadr (assq (cadr x) v-map)) (caddr x))) compound-a))))


(define (DFA-minimize G)
   (define v (vertexes G))
   (define a (arcs G))
   (define l (lex G))
   (define adj-list (map (lambda (x) (list (list (car x) (caddr x)) (cadr x))) a))
   (define acc (map sflip (filter acc? v)))
   (define non-acc (filter (lambda (x) (not (acc? x))) v))
   (define nom-v (append acc non-acc))
   ;(print (list "acc" acc))
   (define (step id ch) 
   		;(print (list "query" id ch)) 
   		(cadr (assq (list id ch) adj-list)))
   ;(print adj-list)
   ;(print "? 2")
   (define (process set)
   		;(print (list "set" set))
   		(define (label cnt set res)
   			(if (null? set) res
   							(label (+ cnt 1) (cdr set) (fold (lambda (x y) (cons (list y cnt) x)) res (car set)))))
   		(define v-map-tmp (label 0 set '()))
   		;(print (list "v-map-tmp" v-map-tmp))
   		(define (succ id)
   			;(print (list "succ" id))
   			;(print (fold (lambda (x y) (cons (cadr (assq (step id y) v-map-tmp)) x)) '() l))
   			;(print "----")
   			(sort (fold (lambda (x y) (cons (cadr (assq (step id y) v-map-tmp)) x)) '() l) <))
   		;(print "ha?")
   		;(define succ-label-list (fold (lambda (x y) (cons (list y (succ y)) x)) '() nom-v))
   		(define (group-label subset) 
   			(define tmp (fold (lambda (x y) (cons (list y (succ y)) x)) '() subset))
   			(map (lambda (x) (list (car x) (append (cdr x) subset))) tmp))
   		(define succ-label-list (fold (lambda (x y) (append (group-label y) x)) '() set))
   		;(print (list "succ" succ-label-list))
   		(define all-labels (remove-duplicates (map cadr succ-label-list)))
   		
   		(define (pick label) (map car (filter (lambda (x) (eq? (cadr x) label)) succ-label-list)))
   		(define set-new (fold (lambda (x y) (cons (pick y) x)) '() all-labels))
   		;(print (list "set-new" set-new))
   		(if (= (length set) (length set-new)) set (process set-new)))
   ;(print (list "nom-v" nom-v))
   ;(define compound-v (process (list acc non-acc)))
   (define old-st (if (eqs? (car nom-v) (car v)) (car nom-v) (car v)))
   (define tmp-v (process (list acc non-acc)))
   ;(print (list "old-st" old-st))
   ;(print (list "tmp-v" tmp-v))
   (define split-v (splitf tmp-v (lambda (x) (fold (lambda (y z) (if (eq? old-st z) #t y)) #f x))))
   ;(print (list "split-v" split-v))
   (define compound-v (append (cadr split-v) (car split-v) (caddr split-v)))
   ;(print (list "compound-v" compound-v))
   (define (convertV T) 
   		;(print T) 
   		(listtoatom T))
   (define (convertVacc T) 
	(define acc-status (fold (lambda (x y) (or x (list? (memq y acc)))) #f T))
	;(print (list "convert" T acc-status))
	(if acc-status (atom-add '# (convertV T)) (convertV T)))

   (define (get-map lst res)
   		(define atom-now (if (null? lst) #t (convertV (car lst))))
   		;(print (list "atom-now" atom-now))
   		(if (null? lst) res
   						(get-map (cdr lst) (append (fold (lambda (x y) (cons (list y atom-now) x)) '() (car lst)) res))))
   (define v-map (get-map compound-v '()))
   ;(print (list "v-map" v-map))
   (define compound-a (remove-duplicates 
   						(fold (lambda (x y) (append 
   												(fold (lambda (z w) (cons (list (convertV y) (cadr (assq (step (car y) w) v-map)) w) z)) '() l) 
   												x))
   								'() compound-v))) 
   (list (map convertVacc compound-v) compound-a))

(define (print x) (write x))
(define (output x str)
   (define P (open-output-file str))
   (write x P)
   (close-output-port P))
(define (paint x) (output (convert x) "output.txt"))

'Done!
