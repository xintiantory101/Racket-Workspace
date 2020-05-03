;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname vectors-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct posn/R3 (x y z))
;; A Posn/R3 is a (make-posn Num Num Num)

;; A Vector is (anyof Posn Posn/R3)


;; (add-vectors/R2 vlst) adds all vectors in vlst.
;; add-vectors/R2: (listof Posn) -> Posn

(define (add-vectors/R2 vlst)
  (local
    [;; (add-x vlst) adds the x-components of all vectors in vlst.
     ;; add-x: (listof Posn) -> Num
     (define (add-x vlst)
       (foldr + 0 (map posn-x vlst)))

     ;; (add-y vlst) adds the y-components of all vectors in vlst.
     ;; add-y: (listof Posn) -> Num
     (define (add-y vlst)
       (foldr + 0 (map posn-y vlst)))]

    (make-posn (add-x vlst) (add-y vlst))))


;; (add-vectors/R3 vlst) adds all vectors in vlst.
;; add-vectors/R3: (listof Posn/R3) -> Posn/R3

(define (add-vectors/R3 vlst)
  (local
    [;; (add-x vlst) adds the x-components of all vectors in vlst.
     ;; add-x: (listof Posn/R3) -> Num
     (define (add-x vlst)
       (foldr + 0 (map posn/R3-x vlst)))

     ;; (add-y vlst) adds the y-components of all vectors in vlst.
     ;; add-y: (listof Posn/R3) -> Num
     (define (add-y vlst)
       (foldr + 0 (map posn/R3-y vlst)))

     ;; (add-z vlst) adds the z-components of all vectors in vlst.
     ;; add-z: (listof Posn/R3) -> Num
     (define (add-z vlst)
       (foldr + 0 (map posn/R3-z vlst)))]

    (make-posn/R3 (add-x vlst) (add-y vlst) (add-z vlst))))


;; (add-vectors vlst) adds all vectors in vlst.
;; add-vectors: (listof Vector) -> Vector
;; requires: all vectors in vlst have the same number of dimensions

(define (add-vectors vlst)
  (cond
    [(posn? (first vlst)) (add-vectors/R2 vlst)]
    [(posn/R3? (first vlst)) (add-vectors/R3 vlst)]))


;; (scalar-multiplication n v) multiplies v by n.
;; scalar-multiplication: Num Vector -> Vector

(define (scalar-multiplication n v)
  (local
    [;; (scalar-multiplication/R2 n v) multiplies v by n.
     ;; scalar-multiplication/R2: Num Posn -> Posn
     (define (scalar-multiplication/R2 n v)
       (make-posn (* n (posn-x v)) (* n (posn-y v))))

     ;; (scalar-multiplication/R3 n v) multiplies v by n.
     ;; scalar-multiplication/R3: Num Posn/R3 -> Posn/R3
     (define (scalar-multiplication/R3 n v)
       (make-posn/R3 (* n (posn/R3-x v)) (* n (posn/R3-y v))
                     (* n (posn/R3-z v))))]

    (cond
      [(posn? v) (scalar-multiplication/R2 n v)]
      [(posn/R3? v) (scalar-multiplication/R3 n v)])))


;; (dot-product/R2 v1 v2) determines the dot product of v1 and v2.
;; dot-product/R2: Posn Posn -> Num

(define (dot-product/R2 v1 v2)
  (+ (* (posn-x v1) (posn-x v2)) (* (posn-y v1) (posn-y v2))))


;; (dot-product/R3 v1 v2) determines the dot product of v1 and v2.
;; dot-product/R3: Posn/R3 Posn/R3 -> Num

(define (dot-product/R3 v1 v2)
  (+ (* (posn/R3-x v1) (posn/R3-x v2))
     (* (posn/R3-y v1) (posn/R3-y v2)) (* (posn/R3-z v1) (posn/R3-z v2))))

;; (dot-product v1 v2) determines the dot product of v1 and v2.
;; dot-product/R3: Vector Vector -> Num
;; requires: v1 and v2 have the same number of dimensions

(define (dot-product v1 v2)
  (cond
    [(posn? v1) (dot-product/R2 v1 v2)]
    [(posn/R3? v1) (dot-product/R3 v1 v2)]))

;; (perpendicular? v1 v2) determines if v1 is perpendicular to v2.
;; perpendicular?: Vector Vector -> Bool
;; requires: v1 and v2 have the same number of dimensions

(define (perpendicular? v1 v2) (zero? (dot-product v1 v2)))


;; (collinear? v1 v2) determines if v1 is parallel to v2.
;; collinear?: Vector Vector -> Bool
;; requires: v1 and v2 have the same number of dimensions

(define (collinear? v1 v2)
  (local
    [;; (collinear?/R2 v1 v2) determines if v1 is parallel to v2.
     ;; collinear?/R2: Posn Posn -> Bool
     (define (collinear?/R2 v1 v2)
       (= (/ (posn-y v1) (posn-x v1)) (/ (posn-y v2) (posn-x v2))))

     ;; (collinear?/R3 v1 v2) determines if v1 is parallel to v2.
     ;; collinear?/R3: Posn/R3 Posn/R3 -> Bool
     (define (collinear?/R3 v1 v2)
       (and (= (/ (posn/R3-y v1) (posn/R3-x v1))
               (/ (posn/R3-y v2) (posn/R3-x v2)))
            (= (/ (posn/R3-z v1) (posn/R3-x v1))
               (/ (posn/R3-z v2) (posn/R3-x v2)))))]

    (cond
      [(posn? v1) (collinear?/R2 v1 v2)]
      [(posn/R3? v1) (collinear?/R3 v1 v2)])))


;; (cross-product v1 v2) determines the cross product of v1 and v2.
;; cross-product: Posn/R3 Posn/R3 -> Posn/R3

(define (cross-product v1 v2)
  (make-posn/R3
   (- (* (posn/R3-y v1) (posn/R3-z v2)) (* (posn/R3-z v1) (posn/R3-y v2)))
   (- (* (posn/R3-z v1) (posn/R3-x v2)) (* (posn/R3-x v1) (posn/R3-z v2)))
   (- (* (posn/R3-x v1) (posn/R3-y v2)) (* (posn/R3-y v1) (posn/R3-x v2)))))


;; (magnitude-v v) determines the magnitude of v.
;; magnitude-v: Vector -> Num

(define (magnitude-v v)
  (local
    [;; (magnitude/R2 v) determines the magnitude of v.
     ;; magnitude/R2: Posn -> Num
     (define (magnitude/R2 v)
       (sqrt (+ (sqr (posn-x v)) (sqr (posn-y v)))))

     ;; (magnitude/R3 v) determines the magnitude of v.
     ;; magnitude/R3: Posn/R3 -> Num
     (define (magnitude/R3 v)
       (sqrt (+ (sqr (posn/R3-x v)) (sqr (posn/R3-y v)) (sqr (posn/R3-z v)))))]

    (cond
      [(posn? v) (magnitude/R2 v)]
      [(posn/R3? v) (magnitude/R3 v)])))


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