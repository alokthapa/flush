#lang scheme

(provide (all-defined-out))
(define *suits* '(H D S C))
(define *ranks* '(A K Q J 10 9 8 7 6 5 4 3 2 ))

(define (make-deck)
  (append-map 
   (lambda (s)
     (map (lambda (r)
	    (cons s r))
	  *ranks*))
   *suits*))
	 
(define (shuffle-deck d)
  (shuffle d))

(define (suit-of c) (car c))
(define (rank-of c) (cdr c))

(define (all-same? fn h)
  (let ((fst (fn (car h))))
    (andmap (lambda (c)
	      (eq? (fn c) fst))
	    (cdr h))))

(define (equal-rank? a b)
  (eq? (rank-of a)
       (rank-of b)))

(define (where lst x)
  (letrec ((recur (lambda (lst n x)
		    (if (eq? (car lst) x)
			n
			(recur (cdr lst) (add1 n) x)))))
    (recur lst 0 x)))

(define (rank-strength c)
  (where *ranks* (rank-of c)))

(define (sort-by-rank h)
  (sort h (lambda (a b)
	    (< (rank-strength a )
	       (rank-strength b)))))

(define (strong-card h)
  (car (sort-by-rank h)))

(define (is-triple? h) (all-same? rank-of h))

(define (is-flush? h) (all-same? suit-of h))

(define (is-doublerun? h) 
  (and (is-flush? h)
       (is-run? h)))

(define (is-run? h)
  (let* ((sorted (sort-by-rank h))
	 (ord (rank-strength (car sorted))))
    (or (and (eq? (rank-of (car sorted)) 'A)
	     (eq? (rank-of (second sorted)) 3)
	     (eq? (rank-of (third sorted)) 2))
	(and (> 11 ord)
	     (eq? (list-ref *ranks* (+ ord 1))
		  (rank-of (second sorted)))
	     (eq? (list-ref *ranks* (+ ord 2))
		  (rank-of (third sorted)))))))

(define (is-double? h)
  (or
   (equal-rank? (first h) (second h))
   (equal-rank? (first h) (third h))
   (equal-rank? (second h) (third h))))

(define (combo-type h)
  (cond 
   ((is-triple? h) 'triple)
   ((is-doublerun? h) 'doublerun)
   ((is-run? h) 'run)
   ((is-flush? h) 'flush)
   ((is-double? h) 'double)
   (else 'highcard)))

(define winrules '(triple doublerun run flush double highcard))

(define (compare-hands h1 h2)
  (let ((t1 (combo-type h1))
	(t2 (combo-type h2)))
    (if (eq? t1 t2)
	(compare-ranks h1 h2)
	(if (< (where winrules t1) (where winrules t2)) h1 h2))))

(define (wins h1 h2)
  (equal? (compare-hands h1 h2) h1))

(define (compare-all-hands lst)
  (foldr compare-hands (car lst) (cdr lst)))

(define (compare-ranks h1 h2)
  (let ((sa (sort-by-rank h1))
	(sb (sort-by-rank h2)))
    (letrec ((recur (lambda (s1 s2)
		      (if (null? s1)
			  'equalstrength
			  (if (equal? (car s1) (car s2))
			      (compare-ranks (cdr s1) (cdr s2))
			      (if (< (rank-strength (car s1)) 
				     (rank-strength (car s2)))
				  h1
				  h2))))))
      (recur sa sb))))

(define tripleA '((H . A) (H . A) (H . A)))
(define arun '((H . 3) (H . 5) (H . 4)))
(define brun '((D . 7) (D . 8) (D . 9)))
(define highA '((D . A) (H . 3) (C . 8)))
(define flushA '((D . 9) (D . 2) (D . 8)))
(define highK '((C . K) (C . 4) (D . 2)))

(define (distribute-cards-given d n)
  (letrec ((recur (lambda (lst dk n)
		    (if (= n 0)
			lst
			(recur (cons (take dk 3) lst) (drop dk 3) (sub1 n))))))
    (recur (list) d n)))

(define (distribute-cards n)
  (distribute-cards-given (shuffle-deck (make-deck)) n  ))

(define (usery/n)
  (display "Please input y/n: ")
  (let ((input (read)))
    (case input
      ((y) #t)
      ((n) #f)
      (else (usery/n)))))

(define (input-play/fold/show)
  (display "Please input p, f or s: " )
  (let ((input (read)))
    (case input
      ((or p f s) input)
      (else (input-play/fold/show)))))

(define (input-user-number text min max)
  (display (string-append text " ( "  (number->string min) " - " (number->string max) "): "))
  (let ((input (read)))
    (if (number? input)
	(if (and (>= input min ) (>= max input ))
	    input
	    (input-user-number text min max))
	(input-user-number text min max))))

(define (input-userbet min  max)
  (input-user-number "How much do you want to bet?" min max))

(define (input-opponents min max)
  (input-user-number "How many opponents?" min max))

(define usercash 500)
(define minbet 10)

(define (remove-hands h d)
  (if (null? h)
      d
      (remove-hands (cdr h) (remove (car h) d))))

(define (winsall hs)
  (filter (lambda (h)
	    (andmap (lambda (opp) (wins h opp)) (remove h hs)))
	  hs))

(define (simulate numb opps hand won)
  (if (= numb 0) 
      won
      (let ((d (remove-hands hand (shuffle-deck (make-deck)))))
	(if (andmap (lambda (opphand) (wins hand opphand)) (distribute-cards-given d opps))
	    (simulate (sub1 numb) opps hand (add1 won))
	    (simulate (sub1 numb) opps hand won)))))

;; willingness to play is directly prop to no of sims won
;; willnigness to play is inversely directly prop to percent of bet. 
(define *sims* 1000)

(define (play? h opps bet cash)
  (< .9 (/ (/ (simulate *sims*  opps h 0) *sims*) (/ bet cash))))

 (define (interval from to)
   (letrec ((r (lambda (lst f t)
		 (if (> f t)
		     lst
		     (r (cons t lst) f (sub1 t))))))
     (r '() from to)))

 (define (gamep)
   (display "Welcome to Flush")
   (let ((opps (input-opponents 3 7)))
     (let ((money (map (lambda (n) 500) (interval 0 opps))))
       (round money))))

(define (round money)
  (let ((hs (distribute-cards (length money)))
	(pot 0)
	(minbet 20)
	(playing? (map (lambda (m) #t) money)))
    (letrec ((playermove 
	      (lambda (pt mbet p? ms h)
		(when (< (stillin p?) 2)
		    (round (map (lambda (p m) (if p (+ m pot) m)) p? ms)))
		(if (not (car p?))
		    (computermove pt
				  mbet
				  (cdr hs)
				  p?
				  ms
				  (cons #f '())
				  (cons (car ms) '()))
		    (begin
		      (display "\nyour hand")
		      (display h)
		      (display "\ncash in hand: ")
		      (display (number->string (car ms)))
		      (display "\nsize of pot: ")
		      (display (number->string pt))
		      (display "\n number of players:")
		      (display (number->string (stillin p?)))
		      (if (< (car ms) mbet)
			  (begin
			    (display "\nsorry not enough money")
			    (computermove pt
					  mbet
					  (cdr hs)
					  p?
					  ms
					  (cons #f '())
					  (cons (car ms) '())))
			  (begin 
			    (display "\nDo you want to [p]lay, [f]old or (side)[s]how?")
			    (case (input-play/fold/show)
			      ((p) 
			       (let ((ubet (input-userbet mbet (car ms))))
				 (computermove (+ pt ubet) 
					       ubet 
					       (cdr hs) 
					       p?
					       ms
					       (cons #t '())
					       (cons (- (car ms) ubet) '()))))
			      ((f) (computermove pt
						 mbet
						 (cdr hs)
						 p?
						 ms
						 (cons #f '())
						 (cons (car ms) '())))
			      ((s) 
			       (let ((winn (winsall hs)))
				 (display "\n The winning hand: ")
				 (display winn)
				 (round (map (lambda (m h)
					       (if (eq? h winn)
						   (+ m pot)
						   m))
					     ms hs)))))))))))
	     (stillin (lambda (p?) (length (filter (lambda (x) x) p?))))
	     (computermove 
		 (lambda (pt mbet hds p? ms newplaying? newms)
		   (if  (null? hds) 
			(playermove pt mbet (reverse newplaying?) (reverse newms) (car hs))
			(if (car p?)
			    (if (and (> (car ms) mbet) (play? (car hds) (stillin p?) mbet (car ms)))
				(begin
				  (display "\ncomputer played.")
				  (computermove (+ pt mbet) 
						mbet
						(cdr hds)
						(cdr p?)
						(cdr ms)
						(cons #t newplaying?)
						(cons (- (car ms) mbet) newms)))
				(begin
				  (display "\ncomputer folded.")
				  (computermove pt
						mbet
						(cdr hds)
						(cdr p?)
						(cdr ms)
						(cons #f newplaying?)
						(cons (car ms) newms))))
			    (computermove pt
					  mbet
					  (cdr hds)
					  (cdr p?)
					  (cdr ms)
					  (cons #f newplaying?)
					  (cons (car ms) newms)))))))
      (playermove pot minbet playing? money (car hs)))))


	       
		 
		     

