;    Copyright 2019 Alexander Eckhart
;
;    This file is part of scheme-oxide.
;
;    Scheme-oxide is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    Scheme-oxide is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with scheme-oxide.  If not, see <https://www.gnu.org/licenses/>.

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
