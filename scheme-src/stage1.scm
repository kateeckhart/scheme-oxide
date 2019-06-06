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
