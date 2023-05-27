#lang racket

;;;
;;; 15 TILES
;;
;; The goal of this game is to put the tiles in order from 1-15
;; with the blank at the bottom right. The tiles move by mouse click.
;; Pressing the space bar will generate a new game.
;;;

(require 2htdp/universe
         2htdp/image
         utils/2htdp/clicker)

(struct world (init? blank-cname blank-bname) #:mutable #:transparent)


(define (key-handler ws ke)
  (cond
    [(and (key=? ke " ") (false? (world-init? ws)))
     (set-world-init?! ws #t)
     ws]                   
    [(key=? ke " ")
     (new-world #t)]
    [else ws]))

(define (mouse-handler ws x y evt)
  (unless (solved? ws) (process-containers CONTAINERS ws x y evt))  
  ws)

(define (move c b ws x-pos y-pos)
  (define cname (container-name c))
  (define bname (button-name b))
  (define blank-cname (world-blank-cname ws))
  (define blank-bname (world-blank-bname ws))
  (define lst (find-container/button blank-cname blank-bname CONTAINERS))
  (cond
    [(false? lst) (error "Blank container/button not found.")]
    [(not (zero? (* (- cname blank-cname) (- bname blank-bname)))) (void)]
    [(and (= cname blank-cname) (= bname blank-bname)) (void)]
    [(and (= cname blank-cname) (> (abs (- bname blank-bname)) 1)) (void)]
    [(and (= bname blank-bname) (> (abs (- cname blank-cname)) 1)) (void)]
    [else
     (define blank-btn (second lst))
     (define blank-label (button-label blank-btn))
     (set-button-label! blank-btn (button-label b))
     (set-button-label!  b blank-label)
     (set-world-blank-cname! ws (container-name c))
     (set-world-blank-bname! ws (button-name b))]))

;; 15-tiles Label defaults
(current-label-border? #t)
(current-label-font-color 'white)
(current-label-bg-color 'black)
(current-label-bo-color 'white)
(current-label-padding 12)

;; 15-tiles Button defaults
(current-button-up-action move)

;; 15-tiles Container defaults
(current-container-button-width 100)
(current-container-button-height 100)

;; Build the containers list used by clicker functions.
;; This builds the container, button, and label configuraiton
;; in a solved state, which is not used except for the basis
;; of button assignmennt by new-world.
(define CONTAINERS
  (for/list ([c (range 4)])
    (make-container (name c)
                    (y-offset (* c (current-container-button-height)))
                    (buttons (for/list ([b (range 4)])
                               (define lbl-name (modulo (+ (* c 4) (add1 b)) 16))
                               (make-button (name b)
                                            (label (if (zero? lbl-name)
                                                       (make-label
                                                        (name lbl-name)
                                                        (font-color 'transparent)
                                                        (bg-color 'white))
                                                       (make-label
                                                    (name lbl-name))))))))))

(define (solvable? ns (acc 0))
  (cond
    [(empty? ns) (even? acc)]
    [(odd? (count (λ (n) (> (car ns) n)) (cdr ns)))
     (solvable? (cdr ns) (add1 acc))]
    [else (solvable? (cdr ns) acc)]))

(define (make-solvable-list)
  (define ordered (range 1 16))
  (define shuffled (shuffle ordered))
  (cond
    [(and (not (equal? shuffled ordered)) (solvable? shuffled)) shuffled]
    [else (make-solvable-list)]))

;; A puzzle is solved when the tiles are put in order from 1 - 15
;; left to right, top to bottom.
(define (solved? ws)
  (define ns
    (drop-right (for*/list ([cname (range 4)]
                            [bname (range 4)])
                  (define c/b (find-container/button cname bname CONTAINERS))
                  (define b (second c/b))
                  (label-name (button-label b)))
                1))
  (equal? ns (range 1 16)))

;; Make a new solvable puzzle, assign to the associated
;; buttons and reuturn the new world state. 
(define (new-world (init #f))
  (define vs (make-solvable-list))
  (for ([v vs]
        [n (in-naturals)])
    [define cname (quotient n 4)]
    [define bname (modulo n 4)]
    [define c/b (find-container/button cname bname CONTAINERS)]
    [unless (false? c/b)
      (define b (second c/b))      
      (define lbl (make-label (name v)))
      (set-button-label! b lbl)])
  [define c/b (find-container/button 3 3 CONTAINERS)]
  (define b (second c/b))
  (define lbl (make-label (name 0) (font-color 'transparent) (bg-color 'white)))
  (set-button-label! b lbl)  
  (world init 3 3))

;;;
;;; Rendering the puzzle s
;;;

(define MT-WIDTH  400)
(define MT-HEIGHT 400)
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))

(define INIT-FONT-SIZE 40)
(define INIT-FONT-COLOR 'white)
(define INIT-EMPHASIS-COLOR 'gold)
(define INIT
  (above/align "left"
               (text "Press" INIT-FONT-SIZE INIT-FONT-COLOR)
               (text "SPACEBAR" INIT-FONT-SIZE INIT-EMPHASIS-COLOR)
               (text "to begin" INIT-FONT-SIZE INIT-FONT-COLOR)
               (text "a new game." INIT-FONT-SIZE INIT-EMPHASIS-COLOR)))

;; Shows the puzzle in 1 of 3 states: instructions, unsolved, and solved.
(define (render ws)
  (cond
    [(false? (world-init? ws))
     (render-init ws)]
    [(solved? ws)
     (render-solved! ws)]
    [else (render-unsolved ws)]))

;; Show the puzzle instructions.
(define (render-init ws)
  (place-image INIT
               (quotient MT-WIDTH 2)
               (quotient MT-HEIGHT 2)
               MT))

;; Show the unsolved puzzle state.
(define (render-unsolved ws)
  (place-containers CONTAINERS
                    MT))

;; Show the solved puzzle. 
(define (render-solved! ws)
  (define img (text "SOLVED!" 80 'darkred))
  (define win-img (rotate 45 (overlay img
                                      (rectangle (+ 40 (image-width img))
                                                 (+ 4 (image-height img))
                                                 'solid 'gold))))
  (overlay
   win-img
   (render-unsolved ws)))

(big-bang (new-world)
  (on-mouse mouse-handler)
  (on-key key-handler)
  (to-draw render)
  (name "15 TILES PUZZLE"))
