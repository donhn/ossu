;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1a_strings_and_images) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
"apple"
"Ada"
; Wrote the first computer program.
(string-append "Ada" " " "Loveace")

"123" ; This ia string
123 ; This is a number

; (+ 1 "123") ; Error
(+ 1 123) ; OK

(string-length "apple")

; 0-based indexing
(substring "Caribou" 2 4)
(substring "012345678" 2 4) ; From 2, up to but not including 4.

(substring "Caribou" 0 3)