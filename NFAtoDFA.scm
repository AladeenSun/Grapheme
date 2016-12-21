; Sample program for NFA to DFA

(load "graph-lib.scm")

(define (NFAtoDFA G)
   (define v (vertexes G))
   (define a (arcs G))
   (define l (lex G))
   (define acc (map sflip (filter acc? v)))
   (define new-v (normal-v v))
   (define (contain? v ch)
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
   (define (succ T) (foldl (lambda (x y) (if (null? (move T y)) x (cons (list T (epsilon-closure (move T y)) y) x))) '() l))
   (define (process T now tot)
	(define to-add (filter (lambda (x) (eq? (memv x tot) #f)) (map cadr (succ T))))
	(define now-new (append now to-add))
        (define tot-new (append tot to-add))
     	(if (> (length now-new) 1)
     	    (process (cadr now-new) (cdr now-new) tot-new)
            tot-new))
   (define (convertV T)
   		(listtoatom T))
   (define (convertVacc T) 
	(define acc-status (fold (lambda (x y) (or x (list? (memq y acc)))) #f T))
	(if acc-status (atom-add '# (convertV T)) (convertV T)))
   (define compound-v (process closureS (list closureS) (list closureS)))
   (define v-map (map (lambda (x) (list x (convertV x))) compound-v))
   (define compound-a (foldl (lambda (x y) (append (succ y) x)) '() compound-v))
   (cons (map convertVacc compound-v)
   	 (list (map (lambda (x) (list (cadr (assq (car x) v-map)) (cadr (assq (cadr x) v-map)) (caddr x))) compound-a))))

'Done!
