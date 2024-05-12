;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 00_define-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;(define-struct structure name (field names))
(define-struct pos (x y))

;constructor (make-pos 3 6)
(define P1 (make-pos 3 6))
(define P2 (make-pos 2 8))

;selectors pos-x, pos-y
(pos-x P1) ;3
(pos-y P2) ;8

;predicate
(pos? P1)       ;true
(pos? "hello")  ;false