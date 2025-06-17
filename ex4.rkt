#lang racket

(provide (all-defined-out))

(define integers-from
  (lambda (n)
    (cons-lzl n (lambda () (integers-from (+ n 1))))))

(define cons-lzl cons)
(define empty-lzl? empty?)
(define empty-lzl '())
(define head car)
(define tail
  (lambda (lzl)
    ((cdr lzl))))

(define leaf? (lambda (x) (not (list? x))))

;; Signature: map-lzl(f, lz)
;; Type: [[T1 -> T2] * Lzl(T1) -> Lzl(T2)]
(define map-lzl
  (lambda (f lzl)
    (if (empty-lzl? lzl)
        lzl
        (cons-lzl (f (head lzl))
                  (lambda () (map-lzl f (tail lzl)))))))

;; Signature: take(lz-lst,n)
;; Type: [LzL*Number -> List]
;; If n > length(lz-lst) then the result is lz-lst as a List
(define take
  (lambda (lz-lst n)
    (if (or (= n 0) (empty-lzl? lz-lst))
      empty-lzl
      (cons (head lz-lst)
            (take (tail lz-lst) (- n 1))))))

; Signature: nth(lz-lst,n)
;; Type: [LzL*Number -> T]
;; Pre-condition: n < length(lz-lst)
(define nth
  (lambda (lz-lst n)
    (if (= n 0)
        (head lz-lst)
        (nth (tail lz-lst) (- n 1)))))


;;; Q3.1
; Signature: append$(lst1, lst2, cont) 
; Type: [List * List * [List -> T]] -> T
; Purpose: Returns the concatination of the given two lists, with cont pre-processing

(define append$
  (lambda (lst1 lst2 cont)
    (if (empty? lst1)
      (cont lst2)
      (append$ (cdr lst1) lst2  (lambda (res) (cont (cons (car lst1) res)))))))



;;; Q3.2
; Signature: equal-trees$(tree1, tree2, succ, fail) 
; Type: [Tree * Tree * [Tree ->T1] * [Pair->T2]] -> T1 U T2
; Purpose: Determines the structure identity of a given two lists, with post-processing succ/fail


(define equal-trees$
  (lambda (t1 t2 succ fail)
    (cond
      [(and (null? t1) (null? t2))
       (succ '())]

      [(or  (null? t1)
            (null? t2))
       (fail (append t1 t2))]

      [(and (not (list? t1)) (not (list? t2)))
       (succ (cons t1 t2))]

      [(not (list? t1))
       (fail (append (list t1) t2))]
      [(not (list? t2))
       (fail (append t1 (list t2)))]

      [else
       (equal-trees$
         (car t1) (car t2)
         (lambda (head-res)
           (equal-trees$
             (cdr t1) (cdr t2)
             (lambda (tail-res)
               (succ (cons head-res tail-res)))
             fail))
         fail)])))



;;; Q4.1

;; Signature: as-real(x)
;; Type: [ Number -> Lzl(Number) ]
;; Purpose: Convert a rational number to its form as a
;; constant real number
(define as-real
  (lambda (x)
    (cons-lzl x (lambda () (as-real x)))))



;; Signature: ++(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Addition of real numbers
(define ++
  (lambda (x y)
    (cons-lzl (+ (head x) (head y))
              (lambda () (++ (tail x) (tail y))))))

;; Signature: --(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Subtraction of real numbers
(define --
  (lambda (x y)
    (cons-lzl (- (head x) (head y))
              (lambda () (-- (tail x) (tail y))))))

;; Signature: **(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Multiplication of real numbers
(define **
  (lambda (x y)
    (cons-lzl (* (head x) (head y))
              (lambda () (** (tail x) (tail y))))))
;; Signature: //(x, y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Number) ]
;; Purpose: Division of real numbers
(define //
  (lambda (x y)
    (cons-lzl (/ (head x) (head y))
              (lambda () (// (tail x) (tail y))))))


;;; Q4.2.a
;; Signature: sqrt-with(x y)
;; Type: [ Lzl(Number) * Lzl(Number) -> Lzl(Lzl(Number)) ]
;; Purpose: Using an initial approximation `y`, return a 
;; sequence of real numbers which converges into the 
;; square root of `x`
(define sqrt-with
  (lambda (x y)
    (let* (
            (x-div-y (// x y))
            (numer (++ y x-div-y))
            (next (// numer (as-real 2)))
          )
      (cons-lzl y
        (lambda () (sqrt-with x next))))))

;;; Q4.2.b
;; Signature: diag(lzl)
;; Type: [ Lzl(Lzl(T)) -> Lzl(T) ]
;; Purpose: Diagonalize an infinite lazy list
(define diag
  (lambda (lzl)
    (cons-lzl (head (head lzl))
              (lambda () (diag (tail (map-lzl tail lzl)))))))

;;; Q4.2.c
;; Signature: rsqrt(x)
;; Type: [ Lzl(Number) -> Lzl(Number) ]
;; Purpose: Take a real number and return its square root
;; Example: (take (rsqrt (as-real 4.0)) 6) => '(4.0 2.5 2.05 2.0006097560975613 2.0000000929222947 2.000000000000002)
(define rsqrt
  (lambda (x)
    (diag (sqrt-with x x))))




;;; ================================
;;; Chatty tests
;;; ================================

(displayln "===== Q4.1: Lazy List Arithmetic =====")

(displayln (take (as-real 5) 5))
;; Expected: '(5 5 5 5 5)

(displayln (take (++ (as-real 4) (as-real 3)) 6))
;; Expected: '(7 7 7 7 7 7)

(displayln (take (-- (integers-from 10) (integers-from 2)) 5))
;; Expected: '(8 8 8 8 8)

(displayln (take (** (integers-from 2) (integers-from 3)) 4))
;; Expected: '(6 15 28 45)

(displayln (take (// (integers-from 10) (integers-from 1)) 5))
;; Expected: '(10 11/2 4 13/4 14/5)


(displayln "===== Q4.2.a: sqrt-with (lazy list of lazy lists) =====")

(define approx-lists (sqrt-with (as-real 4.0) (as-real 1.0)))
(displayln (map (lambda (lz n) (take lz n))
     (take approx-lists 3)
     (list 5 5 5)))
;; Expected: 3 inner lists getting closer to sqrt(4)


(displayln "===== Q4.2.b: diag (diagonalization) =====")

(define fake-matrix
  (cons-lzl (as-real 1)
    (lambda () (cons-lzl (as-real 2)
      (lambda () (cons-lzl (as-real 3)
        (lambda () (cons-lzl (as-real 4)
          (lambda () fake-matrix)))))))))

(displayln (take (diag fake-matrix) 5))
;; Expected: '(1 2 3 4 5)


(displayln "===== Q4.2.c: rsqrt (final square root) =====")

(displayln (take (rsqrt (as-real 4.0)) 6))
;; Expected: '(4.0 2.5 2.05 2.0006097560975613 2.0000000929222947 2.000000000000002)

(displayln (take (rsqrt (as-real 2.0)) 6))
;; Expected: approx '(2.0 1.5 1.416666... ...)

(displayln (take (rsqrt (as-real 9.0)) 6))
;; Expected: approx '(9.0 5.0 3.4 3.0235... ...)