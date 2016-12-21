; Sample program for NFA to regular expression

(load "graph-lib.scm")

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
