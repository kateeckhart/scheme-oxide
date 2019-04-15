;A test program that calculates primes.

(display 2)
(newline)
(display 3)
(newline)

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
                                (begin
                                    (set-cdr! prime-tail new-tail) 
                                    (display possible-prime)
                                    (newline)
                                    (new-prime new-tail))))))))))
