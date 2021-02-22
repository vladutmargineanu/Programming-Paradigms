#lang racket

(require 2htdp/image)
(require test-engine/racket-tests)

(define SMALLEST 10)
(define SMALLEST-TRIANGLE (triangle SMALLEST "outline" "blue"))

(check-expect (triangles-stack SMALLEST) SMALLEST-TRIANGLE)
(check-expect (triangles-stack (* SMALLEST 2))
              (above SMALLEST-TRIANGLE
                     (beside SMALLEST-TRIANGLE
                             SMALLEST-TRIANGLE)))


;; Recursivitate pe stivă

(define (triangles-stack size)
  (if (<= size SMALLEST)
      (triangle size "outline" "blue")
      (above (triangles-stack (/ size 2))
             (beside (triangles-stack (/ size 2))
                     (triangles-stack (/ size 2))))))

;(triangles-stack 1000)


;; Recursivitate pe coadă

(define (triangles-tail size)
  (triangles-tail-helper SMALLEST-TRIANGLE SMALLEST size))

(define (triangles-tail-helper img i size)
  (if (>= i size)
      img
      (triangles-tail-helper (above img (beside img img))
                             (* i 2)
                             size)))

;(triangles-tail 1000)

(test)