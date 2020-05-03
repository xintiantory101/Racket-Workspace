;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prime-factorization) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (prime-factorization n) produces the prime factorization of n.
;; prime-factorization: Nat -> (listof (list Nat Nat))
;; requires: n > 1
(define (prime-factorization n)
  (local
    [(define (prime?/acc m acc)
       (cond
         [(= m acc) true]
         [(zero? (remainder m acc)) false]
         [else (prime?/acc m (add1 acc))]))

     (define (prime? m)
       (prime?/acc m 2))

     (define (prime-fact/acc m acc)
       (cond
         [(> acc m) empty]
         [(or (not (prime? acc))
              (not (zero? (remainder m acc))))
          (prime-fact/acc m (add1 acc))]
         [else (cons acc (prime-fact/acc (/ m acc) acc))]))

     (define (prime-fact m) (prime-fact/acc m 2))

     (define (count-terms lst k)
       (cond
         [(empty? lst) 0]
         [(= k (first lst))
          (add1 (count-terms (rest lst) k))]
         [else (count-terms (rest lst) k)]))

     (define (remove-duplicates lst)
       (cond
         [(empty? lst) empty]
         [(member? (first lst) (rest lst))
          (remove-duplicates (rest lst))]
         [else (cons (first lst) (remove-duplicates (rest lst)))]))

     (define (exponents/acc lst acc)
       (cond
         [(empty? lst) empty]
         [(= acc (first lst))
          (exponents/acc (rest lst) acc)]
         [else (cons (count-terms lst (first lst))
                     (exponents/acc (rest lst) (first lst)))]))

     (define (exponents lst) (exponents/acc lst 0))

     (define (expt-bind lof lox)
       (cond
         [(empty? lox) empty]
         [else (cons (list (first lof) (first lox))
                     (expt-bind (rest lof) (rest lox)))]))]

    (expt-bind (remove-duplicates (prime-fact n))
               (exponents (prime-fact n)))))