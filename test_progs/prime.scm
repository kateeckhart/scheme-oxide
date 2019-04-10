;A test program that calculates primes.

($disp-num 2)
($disp-num 3)

(let ((prime-head (list 3)))
   (let new-prime ((prime-tail prime-head))
     (let test-possible-prime ((possible-prime (+ 2 (car prime-tail))))
       (let test-primes ((prime (car prime-head)) (prime-rest (cdr prime-head)))
	 (if (= 0 (remainder possible-prime prime))
	     (test-possible-prime (+ 2 possible-prime))
	     (cond
	       ((pair? prime-rest) (test-primes (car prime-rest) (cdr prime-rest)))
	       ((null? prime-rest)
                (let ((new-tail (list possible-prime))) 
                  (set-cdr! prime-tail new-tail) 
		  ($disp-num possible-prime)
		  (new-prime new-tail)))))))))
