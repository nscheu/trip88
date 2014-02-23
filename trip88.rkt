;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trip88) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "lab8-teachpack.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "lab8-teachpack.rkt" "installed-teachpacks")))))
(require 2htdp/universe)
(require 2htdp/image)
;                                                                  
;                      ;                      ;      ;;;;    ;;;;  
;    ;                                        ;     ;    ;  ;    ; 
;   ;;;;;   ;; ;;;   ;;;    ;; ;;             ;     ;    ;  ;    ; 
;    ;       ;;        ;     ;;  ;            ;      ;;;;    ;;;;  
;    ;       ;         ;     ;   ;                  ;    ;  ;    ; 
;    ;       ;         ;     ;   ;                  ;    ;  ;    ; 
;    ;   ;   ;         ;     ;   ;                  ;    ;  ;    ; 
;     ;;;   ;;;;;    ;;;;;   ;;;;                    ;;;;    ;;;;  
;                            ;                                     
;                           ;;;                                    
;                                                                  
;                                                                  
; trip '88 is a game where you have 3 columns of numbers. 
; Each column may contain a maximum of [ 16 | 7 | 9 ]
; The point of the game is to move values from one column to another
; in order to wind up with two columns each containing 8 with 
; one column containing 0.

; A gst is one of
; - int - max 16
; - int - max 9
; - int - max 7
(define-struct gst (g16 g7 g9 select))

(define SIZE 800)
(define %50 (* SIZE .5))
(define %75 (* 400 .7))
(define %25 (* 400 1/4))
(define BASE (empty-scene SIZE SIZE))

; 
;                                                                  
;                                                                  
;                                                                  
;                                                                  
;   ;;;;;      ;                    ;;;;;                          
;    ;   ;                           ;   ;                         
;    ;   ;   ;;;     ;;; ;;          ;   ;   ;;;;   ;; ;;    ;;; ;;
;    ;;;;      ;    ;   ;;           ;;;;   ;    ;   ;;  ;  ;   ;; 
;    ;   ;     ;    ;    ;           ;   ;   ;;;;;   ;   ;  ;    ; 
;    ;   ;     ;    ;    ;           ;   ;  ;    ;   ;   ;  ;    ; 
;    ;   ;     ;    ;   ;;           ;   ;  ;   ;;   ;   ;  ;   ;; 
;   ;;;;;    ;;;;;   ;;; ;          ;;;;;    ;;; ;; ;;; ;;;  ;;; ; 
;                        ;                                       ; 
;                    ;;;;                                    ;;;;  
;                                                                  
;                                                                  

; Number -> Image
(define (trip88 player-name) 
  (local ((define (call-draw-game gs) (draw-game gs player-name)))
    (big-bang (make-gst 16 0 0 0)
              [on-key get-keystroke]
              [to-draw call-draw-game] 
              [stop-when game-over game-over-image]
              )))

; gst String -> Image
; Renders the game state as an image
(define (draw-game gs name) 
  ;(make-game-slots 16)
  (place-image 
   (overlay/offset 
    (text " (16) (7) (9) Columns Max Value" 25 "blue") 0 -10
    (overlay/offset 
     (text "select and move using the 'left' 'down' 'right' 
arrow keys" 25 "blue") 0 -80
                       (text (string-append ": "(number->string (gst-g16 gs)) 
                                            " : " 
                                            (number->string (gst-g7 gs)) 
                                            " : "
                                            (number->string (gst-g9 gs)) 
                                            " :: Column Selected "
                                            (number->string (gst-select gs)))
                             25 "blue"))) %50 %50 BASE))
; gst Number -> gst
; Sets the value of the select field of g-state
(define (set-select gs n)
  (make-gst (gst-g16 gs) (gst-g7 gs) (gst-g9 gs) n))
(check-expect (set-select (make-gst 4 3 9 0) 1) (make-gst 4 3 9 1))

; Num Num Num -> Boolean
; is there room? destination-value destination-size source-value
(define (isRoom? destval destsize srcval)
  (> (+ destval srcval) destsize))

; gst -> gst
; moves values from 7 to 16
(define (move7to16 gs)
  (make-gst (+ (gst-g16 gs) (gst-g7 gs)) 0 (gst-g9 gs) 0))
(check-expect (move7to16 (make-gst 0 7 9 2)) (make-gst 7 0 9 0))
(check-expect (move7to16 (make-gst 5 2 9 2)) (make-gst 7 0 9 0))
(check-expect (move7to16 (make-gst 6 1 9 2)) (make-gst 7 0 9 0))
(check-expect (move7to16 (make-gst 13 1 2 2)) (make-gst 14 0 2 0))
(check-expect (move7to16 (make-gst 15 1 0 2)) (make-gst 16 0 0 0))

; gst -> gst
; moves values from 9 to 16
(define (move9to16 gs)
  (make-gst (+ (gst-g16 gs) (gst-g9 gs)) (gst-g7 gs) 0 0))
(check-expect (move9to16 (make-gst 0 7 9 3)) (make-gst 9 7 0 0))
(check-expect (move9to16 (make-gst 8 7 1 3)) (make-gst 9 7 0 0))
(check-expect (move9to16 (make-gst 0 7 9 3)) (make-gst 9 7 0 0))
(check-expect (move9to16 (make-gst 7 0 9 3)) (make-gst 16 0 0 0))

; gst -> gst
; moves values from 16 to 7
(define (move16to7 gs)
  (if (isRoom? (gst-g7 gs) 7 (gst-g16 gs))
      (make-gst (- (gst-g16 gs) (- 7 (gst-g7 gs)))  
                ;(- (gst-g16 gs) (- (gst-g16 gs) (- 7 (gst-g7 gs))))
                7
                (gst-g9 gs) 0)
      (make-gst 0  (+ (gst-g7 gs) (gst-g16 gs)) (gst-g9 gs) 0)))
(check-expect (move16to7 (make-gst 7 0 9 1)) (make-gst 0 7 9 0))
(check-expect (move16to7 (make-gst 16 0 0 1)) (make-gst 9 7 0 0))
(check-expect (move16to7 (make-gst 14 2 0 1)) (make-gst 9 7 0 0))
(check-expect (move16to7 (make-gst 4 3 9 1)) (make-gst 0 7 9 0))
(check-expect (move16to7 (make-gst 10 3 3 1)) (make-gst 6 7 3 0))
(check-expect (move16to7 (make-gst 9 0 7 1)) (make-gst 2 7 7 0))

; gst -> gst
; moves values from 9 to 7
(define (move9to7 gs)
  (if (isRoom? (gst-g7 gs) 7 (gst-g9 gs))
      (make-gst (gst-g16 gs) 7 (- (+ (gst-g7 gs) (gst-g9 gs)) 7) 0)
      (make-gst (gst-g16 gs) (+ (gst-g7 gs) (gst-g9 gs)) 0 0)))
(check-expect (move9to7 (make-gst 4 3 9 1)) (make-gst 4 7 5 0))
(check-expect (move9to7 (make-gst 1 6 9 1)) (make-gst 1 7 8 0))
(check-expect (move9to7 (make-gst 0 7 9 1)) (make-gst 0 7 9 0))
(check-expect (move9to7 (make-gst 8 7 1 1)) (make-gst 8 7 1 0))
(check-expect (move9to7 (make-gst 10 3 3 1)) (make-gst 10 6 0 0))


; gst -> gst
; moves values from 7 to 9
(define (move7to9 gs)
  (if (isRoom? (gst-g9 gs) 9 (gst-g7 gs))
      (make-gst (gst-g16 gs) (gst-g7 gs) (gst-g9 gs) 0)
      (make-gst (gst-g16 gs) 0 (+ (gst-g9 gs) (gst-g7 gs)) 0)))
(check-expect (move7to9 (make-gst 4 3 9 1)) (make-gst 4 3 9 0))
(check-expect (move7to9 (make-gst 8 3 5 1)) (make-gst 8 0 8 0))
(check-expect (move7to9 (make-gst 9 7 0 1)) (make-gst 9 0 7 0))
(check-expect (move7to9 (make-gst 0 7 9 1)) (make-gst 0 7 9 0))

; gst -> gst
; moves values from 16 to 9
(define (move16to9 gs)
  (if (isRoom? (gst-g9 gs) 9 (gst-g16 gs))
      (make-gst (- (gst-g16 gs) (- 9 (gst-g9 gs))) (gst-g7 gs) 9 0)
      (make-gst 0 (gst-g7 gs) (gst-g16 gs) 0)))
(check-expect (move16to9 (make-gst 4 3 9 1)) (make-gst 4 3 9 0))
(check-expect (move16to9 (make-gst 9 7 0 1)) (make-gst 0 7 9 0))
(check-expect (move16to9 (make-gst 8 3 5 1)) (make-gst 4 3 9 0))
(check-expect (move16to9 (make-gst 16 0 0 1)) (make-gst 7 0 9 0))
(check-expect (move16to9 (make-gst 7 0 9 1)) (make-gst 7 0 9 0))

; gst Number Number MouseEvent -> gst
;(define (click-handler state x y event) ...)
(define (get-keystroke gs event) 
  (cond
    [(and (key=? event "left") (= (gst-select gs) 0)) (set-select gs 1)]
    [(and (key=? event "down") (= (gst-select gs) 0)) (set-select gs 2)]
    [(and (key=? event "right") (= (gst-select gs) 0)) (set-select gs 3)]
    [(and (key=? event "left") (= (gst-select gs) 2)) (move7to16 gs)]
    [(and (key=? event "left") (= (gst-select gs) 3)) (move9to16 gs)]
    [(and (key=? event "down") (= (gst-select gs) 1)) (move16to7 gs)]
    [(and (key=? event "down") (= (gst-select gs) 3)) (move9to7 gs)]
    [(and (key=? event "right") (= (gst-select gs) 1)) (move16to9 gs)]
    [(and (key=? event "right") (= (gst-select gs) 2)) (move7to9 gs)]
    [else gs]
    ))
(check-expect (get-keystroke (make-gst 16 0 0 0) "left") (make-gst 16 0 0 1))
(check-expect (get-keystroke (make-gst 16 0 0 0) "down") (make-gst 16 0 0 2))
(check-expect (get-keystroke (make-gst 16 0 0 0) "right") (make-gst 16 0 0 3))
(check-expect (get-keystroke (make-gst 9 7 0 2) "left") (make-gst 16 0 0 0))
(check-expect (get-keystroke (make-gst 4 4 8 3) "left") (make-gst 12 4 0 0))
(check-expect (get-keystroke (make-gst 16 0 0 1) "down") (make-gst 9 7 0 0))
(check-expect (get-keystroke (make-gst 7 0 9 3) "down") (make-gst 7 7 2 0))
(check-expect (get-keystroke (make-gst 16 0 0 1) "right") (make-gst 7 0 9 0))
(check-expect (get-keystroke (make-gst 9 7 0 2) "right") (make-gst 9 0 7 0))
(check-expect (get-keystroke (make-gst 16 0 0 0) "1") (make-gst 16 0 0 0))

; gst -> Boolean
; Determines if the game has been solved (hint 8 0 8)
(define (game-over gs)
  (and (= (gst-g16 gs) 8) (= (gst-g7 gs) 0) (= (gst-g9 gs) 8)))
(check-expect (game-over (make-gst 8 0 8 1)) true)
(check-expect (game-over (make-gst 16 0 0 1)) false)

; gst -> Image
; Displays the game over message
(define (game-over-image gs) (place-image 
                              (text "You Won! Game is Over!!!" 30 "red") 
                              %50 %50 BASE))