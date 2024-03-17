;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1a_strings_and_images_2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(circle 10 "solid" "red")
(rectangle 30 60 "outline" "blue")
(text "hello" 24 "orange") ; creates an image of text, not a string

; above, beside, overlay
; There are other primitives for images.
(above (circle 10 "solid" "red")
       (circle 20 "solid" "red")
       (circle 30 "solid" "red"))