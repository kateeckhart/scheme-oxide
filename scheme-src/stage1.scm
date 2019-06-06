;   Copyright 2019 Alexander Eckhart
;
;   This file is part of scheme-oxide.
;
;   Scheme-oxide is free software: you can redistribute it and/or modify
;   it under the terms of the GNU General Public License as published by
;   the Free Software Foundation, either version 3 of the License, or
;   (at your option) any later version.
;
;   Scheme-oxide is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU General Public License
;   along with scheme-oxide.  If not, see <https://www.gnu.org/licenses/>.

(define ($new-type-id) ($make-object 0))
(define $immutable-pair-type-id ($new-type-id))
(define $mutable-pair-type-id ($new-type-id))
(define $symbol-type-id ($new-type-id))
(define $empty-list ($new-type-id))

;Quote is not available in stage1 so use false as a placeholder that is replaced in stage2.
(define $car-name #f)
(define $cdr-name #f)
(define $set-car!-name #f)
(define $set-cdr!-name #f)
(define $force-set-car!-name #f)
(define $force-set-cdr!-name #f)

(define (eq? x y) (eqv? x y))
(define (not x) (if x #f #t))
(define (null? x) (eqv? x $empty-list))
(define (boolean? x) (or (eqv? x #t) (eqv? x #f)))
(define (zero? x) (= x 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (abs x) (if (negative? x) (- x) x))

(define ($mutable-pair? x)
    (and ($object? x) (eqv? ($object-type-id-get x) $mutable-pair-type-id)))
(define (pair? x)
    (or ($mutable-pair? x) (and ($object? x) (eqv? ($object-type-id-get x) $immutable-pair-type-id))))
(define ($assert-pair name x) (if (not (pair? x)) (error name "Not a pair." x)))
(define ($assert-mutable-pair name x) (if (not ($mutable-pair? x)) (error name "Not a mutable pair." x)))

(define (car x) ($assert-pair $car-name x) ($object-field-get x 0))
(define (cdr x) ($assert-pair $cdr-name x) ($object-field-get x 1))
(define ($force-set-car! x y) ($assert-pair $force-set-car!-name x) ($object-field-set! x 0 y))
(define ($force-set-cdr! x y) ($assert-pair $force-set-cdr!-name x) ($object-field-set! x 1 y))
(define (set-car! x y) ($assert-mutable-pair $set-car!-name x) ($force-set-car! x y))
(define (set-cdr! x y) ($assert-mutable-pair $set-cdr!-name x) ($force-set-cdr! x y))
(define (cons x y) ($make-object $mutable-pair-type-id x y))

;This function produces a pair of functions
;The function in car takes a single argument that is put at the end of an list 
;The function in cdr takes one argument and makes that arguemnt the tail of the list then returns it
;Arguments: mutable? - determines if you can mutate the returned list
(define ($make-list-factory mutable?)
    (let ((type-id (if mutable? $mutable-pair-type-id $immutable-pair-type-id))
            (partial-list-head $empty-list)
            (partial-list-tail $empty-list))
        (cons
            (lambda (x)
                (let ((new-tail ($make-object type-id x $empty-list)))
                    (if (null? partial-list-head)
                        (set! partial-list-head new-tail)
                        ($force-set-cdr! partial-list-tail new-tail))
                    (set! partial-list-tail new-tail)))
            (lambda (x)
                (if (null? partial-list-head)
                    x
                    (begin
                        ($force-set-cdr! partial-list-tail x)
                        partial-list-head))))))

(define equal? #f)
(set! equal? (lambda (x y)
    (cond
        ((eqv? x y))
        ((and (pair? x) (pair? y)) (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        (else #f))))
