;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname 00_list_abbreviations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(cons "a" (cons "b" (cons "c" empty)))

(list "a" "b" "c")

(list (+ 1 2) (+ 3 4) (+ 5 6))

(define L1 (list "b" "c"))
(define L2 (list "d" "e" "f"))

(cons "a" L1) ; produces "a" as first element and uses the ELEMENTS from L1 to construct the rest of the list

(list "a" L1) ; produces "a" as first element, and another List as the second element

(list L1 L2)

(append L1 L2) ; expects two lists

; (append "a" L1) ; this doesn't work

