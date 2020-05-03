;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname vectors) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A VectorR2 is a (list Num Num)

;; A VectorR3 is a (list Num Num Num)

;; A Vector is one of:
;; * A VectorR2
;; * A VectorR3

;; (add-vectors vlst) adds all vectors in vlst.
;; add-vectors: (listof Vector) -> Vector
;; requires: all vectors in vlst have the same number of dimensions

(define (add-vectors vlst)
  (local
    [;; (add-vectors/R2 vlst x y) adds all vectors in vlst, with
     ;;   x and y as accumulators.
     ;; add-vectors/R2: (listof VectorR2) Num Num -> VectorR2
     (define (add-vectors/R2 vlst x y)
       (cond
         [(empty? vlst) (list x y)]
         [else
          (add-vectors/R2 (rest vlst) (+ (first (first vlst)) x)
                          (+ (second (first vlst)) y))]))

     ;; (add-vectors/R3 vlst x y z) adds all vectors in vlst, with
     ;;   x, y, and z as accumulators.
     ;; add-vectors/R3: (listof VectorR3) Num Num Num -> VectorR3
     (define (add-vectors/R3 vlst x y z)
       (cond
         [(empty? vlst) (list x y z)]
         [else
          (add-vectors/R3 (rest vlst) (+ (first (first vlst)) x)
                          (+ (second (first vlst)) y)
                          (+ (third (first vlst)) z))]))]

    (cond
      [(empty? vlst) empty]
      [(= (length (first vlst)) 2) (add-vectors/R2 vlst 0 0)]
      [(= (length (first vlst)) 3) (add-vectors/R3 vlst 0 0 0)])))


;; (scalar-multiplication n v) multiplies v by n.
;; scalar-multiplication: Num Vector -> Vector

(define (scalar-multiplication n v)
  (map (lambda (x) (* n x)) v))


;; (dot-product v1 v2) produces the dot product of v1 and v2.
;; dot-product: Vector Vector -> Num
;; requires: v1 and v2 must have the same number of dimensions

(define (dot-product v1 v2)
  (cond
    [(empty? v1) 0]
    [else (+ (* (first v1) (first v2)) (dot-product (rest v1) (rest v2)))]))


;; (perpendicular? v1 v2) determines whether or not v1 is perpendicular to v2.
;; perpendicular?: Vector Vector -> Bool
;; requires: v1 and v2 must have the same number of dimensions

(define (perpendicular? v1 v2) (zero? (dot-product v1 v2)))


;; (collinear? v1 v2) determines whether or not v1 and v2 are collinear.
;; collinear?: Vector Vector -> Bool
;; requires: v1 and v2 must have the same number of dimensions

(define (collinear? v1 v2)
  (cond
    [(= (length v1) 2)
     (= (/ (second v1) (first v1)) (/ (second v2) (first v2)))]
    [(= (length v1) 3)
     (and (= (/ (second v1) (first v1)) (/ (second v2) (first v2)))
          (= (/ (third v1) (first v1)) (/ (third v2) (first v2))))]))


;; (cross-product v1 v2) produces the cross product of v1 and v2.
;; cross-product: VectorR3 VectorR3 -> VectorR3

(define (cross-product v1 v2)
  (list (- (* (second v1) (third v2)) (* (third v1) (second v2)))
        (- (* (third v1) (first v2)) (* (first v1) (third v2)))
        (- (* (first v1) (second v2)) (* (second v1) (first v2)))))


;; (magnitude-v v) produces the magnitude of v.
;; magnitude-v: Vector -> Num

(define (magnitude-v v)
  (local
    [;; (sum-of-squares lst) produces the sum of the squares of the
     ;;   elements of lst.
     ;; sum-of-squares: Vector -> Num
     (define (sum-of-squares lst)
       (foldr + 0 (map sqr lst)))]

    (sqrt (sum-of-squares v))))


;; (unit-vector n v direction) produces a vector in the consumed direction of v,
;;   with length n.
;; unit-vector: Num Vector (anyof 'same 'opposite) -> Vector

(define (unit-vector n v direction)
  (cond
    [(symbol=? direction 'same)
     (scalar-multiplication (/ n (magnitude-v v)) v)]
    [(symbol=? direction 'opposite)
     (scalar-multiplication (- (/ n (magnitude-v v))) v)]))


;; (angle-rad v1 v2) produces the non-reflex angle between v1 and v2 in radians.
;; angle-rad: Vector Vector -> Num
;; requires: v1 and v2 must each have a positive magnitude and must have the
;;           same number of dimensions

(define (angle-rad v1 v2)
  (acos (/ (dot-product v1 v2) (* (magnitude-v v1) (magnitude-v v2)))))


;; (angle-deg v1 v2) produces the non-reflex angle between v1 and v2 in degrees.
;; angle-deg: Vector Vector -> Num
;; requires: v1 and v2 must each have a positive magnitude and must have the
;;           same number of dimensions

(define (angle-deg v1 v2) (* (angle-rad v1 v2) (/ 180 pi)))