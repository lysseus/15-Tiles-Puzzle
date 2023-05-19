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
  (when (key=? ke " ")
    (if (false? (world-init? ws))
        (set-world-init?! ws #t)
        (new-world)))
  ws)

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
    (map string->number
         (drop-right (for*/list ([cname (range 4)]
                                 [bname (range 4)])
                       (define c/b (find-container/button cname bname CONTAINERS))
                       (define b (second c/b))
                       (label-name (button-label b)))
                     1)))
  (equal? ns (range 1 16)))

;;;
;;; Rendering
;;;

(define FONT-SIZE 40)
(define FONT-COLOR 'white)
(define EMPHASIS-COLOR 'gold)

(define MT-WIDTH  400)
(define MT-HEIGHT 400)
(define MT (empty-scene MT-WIDTH MT-HEIGHT 'black))
(define INIT
  (above/align "left"
               (text "Press" FONT-SIZE FONT-COLOR)
               (text "SPACEBAR" FONT-SIZE EMPHASIS-COLOR)
               (text "to begin" FONT-SIZE FONT-COLOR)
               (text "a new game." FONT-SIZE FONT-COLOR)))

(define btn-w 100)
(define btn-h 100)

(define LABEL-PAD 8)

(define (make-label n)
  (define FONT-COLOR 'white)
  (define BG-COLOR 'black)
  (define BO-COLOR 'white)
  (define BLANK-FONT-COLOR 'transparent)
  (define BLANK-BG-COLOR  'white)
  (define BLANK-BO-COLOR 'white)
  (cond
    [(zero? n)
     (label " " #t #f BLANK-FONT-COLOR BLANK-BG-COLOR BLANK-BG-COLOR LABEL-PAD)]
    [else
     (label (~a n) #t #f FONT-COLOR BG-COLOR BO-COLOR LABEL-PAD)]))

(define (make-button btn-name lbl-name)
  (button btn-name
          #t
          (make-label lbl-name)
          move))

(define (make-container ctn-name . btns)
  (container ctn-name
             #t
             0
             (* ctn-name btn-h)
             'transparent
             #f
             #f
             #f
             #f
             #f
             0
             0
             btn-w
             btn-h
             #f
             #f
             #f
             btns))

(define CONTAINERS
  (list
   (make-container 0
                   (make-button 0 1)
                   (make-button 1 2)
                   (make-button 2 3)
                   (make-button 3 4))
   (make-container 1
                   (make-button 0 5)
                   (make-button 1 6)
                   (make-button 2 7)
                   (make-button 3 8))
   (make-container 2
                   (make-button 0 9)
                   (make-button 1 10)
                   (make-button 2 11)
                   (make-button 3 12))
   (make-container 3
                   (make-button 0 13)
                   (make-button 1 14)
                   (make-button 2 15)
                   (make-button 3 0))))

(define (render ws)
  (cond
    [(false? (world-init? ws))
     (render-init ws)]
    [(solved? ws)
     (render-solved! ws)]
    [else (render-unsolved ws)]))

(define (render-init ws)
  (place-image INIT
               (quotient MT-WIDTH 2)
               (quotient MT-HEIGHT 2)
               MT))

(define (render-unsolved ws)
  (place-containers CONTAINERS
                    MT))

(define (render-solved! ws)
  (define img (text "SOLVED!" 80 'darkred))
  (define win-img (rotate 45 (overlay img
                                      (rectangle (+ 40 (image-width img))
                                                 (+ 4 (image-height img))
                                                 'solid 'gold))))
  (overlay
   win-img
   (render-unsolved ws)))

(define (new-world)
  (define vs (make-solvable-list))
  (for ([v vs]
        [n (in-naturals)])
    [define cname (quotient n 4)]
    [define bname (modulo n 4)]
    [define c/b (find-container/button cname bname CONTAINERS)]
    [unless (false? c/b)
      (define b (second c/b))
      (define lbl (label (~a v) #t #f 'white 'black 'white LABEL-PAD))
      (set-button-label! b lbl)])
  (world #f 3 3))

(big-bang (new-world)
  (on-mouse mouse-handler)
  (on-key key-handler)
  (to-draw render)
  (name "15 TILES PUZZLE"))
