;A test program that calculates primes.

($disp_num 2)
($disp_num 3)

(let ((prime_head (list 3)))
   (let new_prime ((prime_tail prime_head))
     (let test_possible_prime ((possible_prime (+ 2 (car prime_tail))))
       (let test_primes ((prime (car prime_head)) (prime_rest (cdr prime_head)))
	 (if (= 0 (remainder possible_prime prime))
	     (test_possible_prime (+ 2 possible_prime))
	     (cond
	       ((pair? prime_rest) (test_primes (car prime_rest) (cdr prime_rest)))
	       ((null? prime_rest)
		(let ((new_tail (list possible_prime))) 
		  (set_cdr! prime_tail new_tail) 
		  ($disp_num possible_prime)
		  (new_prime new_tail)))))))))
