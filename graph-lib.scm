; Graph library for Grapheme

(load "grapheme.scm")
(load "list-lib.scm")

; Fetch some items from G

(define (vertexes G) (car G))
(define (arcs G) (cadr G))
(define (vertex G pos) (list-ref (vertexes G) pos))
(define (arc G pos) (list-ref (arcs G) pos))

;Relabel the vertexes of G by capitals
(define (relabel G)
	(define UPPER '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
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

;The alphabet of G

(define (lex G) 
	(filter (lambda (x) (not (eq? x '#))) (remove-duplicates (flatten (map (compose atom-split caddr) (arcs G))))))

; Drop the leading # of vertexes

(define (normal-v v) (map (lambda (x) (if (acc? x) (sflip x) x)) v))
