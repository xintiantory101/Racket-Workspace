;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fibonacci) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (fibonacci n) produces the first n terms of the Fibonacci sequence.
;; fibonacci: Nat -> (listof Nat)
;; Examples:
(check-expect (fibonacci 4) (list 1 1 2 3))
(check-expect (fibonacci 7) (list 1 1 2 3 5 8 13))

(define (fibonacci n)
  (local
    [;; (fibonacci/acc m a b) produces the first m terms of the Fibonacci
     ;;   sequence, with a and b as accumulators.
     ;; fibonacci/acc: Nat Nat Nat -> (listof Nat)
     (define (fibonacci/acc m a b)
       (cond
         [(not (positive? m)) empty]
         [(odd? m)
          (cons a (fibonacci/acc (sub1 m) (+ a b) b))]
         [(even? m)
          (cons b (fibonacci/acc (sub1 m) a (+ a b)))]))]

    (fibonacci/acc n 1 1)))

;; Tests:
(check-expect (fibonacci 0) empty)
(check-expect (fibonacci 1) (list 1))
(check-expect (fibonacci 2) (list 1 1))
(check-expect (fibonacci 3) (list 1 1 2))
(check-expect (fibonacci 4) (list 1 1 2 3))
(check-expect (fibonacci 5) (list 1 1 2 3 5))