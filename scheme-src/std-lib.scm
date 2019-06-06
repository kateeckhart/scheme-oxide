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

(set! $car-name 'car)
(set! $cdr-name 'cdr)
(set! $set-car!-name 'set-car!)
(set! $set-cdr!-name 'set-cdr!)
(set! $force-set-car!-name '$force-set-car!)
(set! $force-set-cdr!-name '$force-set-cdr!)

(define (symbol? x) (and ($object? x) (eqv? ($object-type-id-get x) $symbol-type-id)))
(define (symbol->string x)
    (if (symbol? x)
        ($object-field-get x 0)
        (error 'symbol->string "Not a symbol.")))

(define (list . lst) lst)
(define (max x . in-rest)
    (let max ((x x) (rest in-rest))
        (if (null? rest)
            x
            (let ((y (car rest)) (new-rest (cdr rest)))
                (if (< x y)
                    (max y new-rest)
                    (max x new-rest))))))
(define (min x . in-rest)
    (let min ((x x) (rest in-rest))
        (if (null? rest)
            x
            (let ((y (car rest)) (new-rest (cdr rest)))
                (if (< x y)
                    (min x new-rest)
                    (min y new-rest))))))
(define ($string-copy-onto! src dest size)
    (if (or (> size (string-length src)) (> size (string-length dest)))
        (error '$string-copy-onto "Size is greater than length.")
        (let copy-onto ((index 0))
            (if (= index size)
                (if #f #f)
                (let ((char (string-ref src index)))
                    (string-set! dest index char)
                    (copy-onto (+ index 1)))))))
(define ($string-truncating-copy str size)
    (if (zero? size)
        ""
        (let ((new_str (make-string size)) (chars-to-copy (min size (string-length str))))
            ($string-copy-onto! str new_str chars-to-copy)
            new_str)))
(define (string-copy str)
    ($string-truncating-copy str (string-length str)))
(define (list->string lst)
    (if (null? lst)
        ""
        (let conv-list ((built-string (make-string 1)) (index 0) (lst-head lst))
            (cond
                ((null? lst-head)
                    (if (= (string-length built-string) index)
                        built-string
                        ($string-truncating-copy built-string index)))
                ((= index (string-length built-string))
                    (conv-list ($string-truncating-copy built-string (* 2 index)) index lst-head))
                (else
                    (string-set! built-string index (car lst-head))
                    (conv-list built-string (+ 1 index) (cdr lst-head)))))))
(define (number->string x)
    (if (zero? x)
        (string-copy "0")
        (let to-string ((x x) (chars '()))
            (if (zero? x)
                (list->string chars)
                (let* ((digits "0123456789") (digit (string-ref digits (remainder x 10))) (rest (quotient x 10)))
                    (to-string rest (cons digit chars)))))))
(define display #f)
(set! display (lambda (x)
    (cond
        ((char? x) (write-char x))
        ((null? x) (display "()"))
        ((pair? x)
            (display "(")
            (display (car x))
            (let display-contents ((list (cdr x)))
                (cond
                    ((null? list))
                    ((pair? list)
                        (display " ")
                        (display (car list))
                        (display-contents (cdr list)))
                    (else
                        (display " . ")
                        (display list))))
            (display ")"))
        ((string? x) (let print-str ((index 0))
            (if (= (string-length x) index)
                (if #f #f)
                (begin
                  (write-char (string-ref x index))
                  (print-str (+ 1 index))))))
        ((number? x) (display (number->string x)))
        ((boolean? x) (if x (display "#t") (display "#f")))
        ((symbol? x) (display (symbol->string x)))
        (else (display "#Unwriteable_object")))))
(define (newline) (display $newline-str))

