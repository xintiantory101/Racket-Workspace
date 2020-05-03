;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binom) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (factorial n)
  (local
    [(define factors (build-list n add1))]

    (foldr * 1 factors)))


(define (binom-coeff n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))


(define (pascal-row n)
  (local
    [(define (pascal-row/acc n r)
       (cond
         [(> r n) empty]
         [else (cons (binom-coeff n r) (pascal-row/acc n (add1 r)))]))]

    (pascal-row/acc n 0)))


(define (pascal-triangle n)
  (local
    [(define (pascal-triangle/acc n r)
       (cond
         [(> r n) empty]
         [else (cons (pascal-row r) (pascal-triangle/acc n (add1 r)))]))]

    (pascal-triangle/acc n 0)))


(define (binom-probability n p r)
  (* (binom-coeff n r) (expt p r) (expt (- 1 p) (- n r))))