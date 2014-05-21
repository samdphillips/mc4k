#lang racket/gui
; mc4ka.scm: Simplified Minecraft4k
; This version by Anthony C. Hay - 
;   http://howtowriteaprogram.blogspot.co.uk/
; Based on work by Salvatore Sanfilippo - 
;   https://gist.github.com/4195130
; Which, in turn was based on Markus Persson's Minecraft4k - 
;   http://jsdo.it/notch/dB1E
; This code is public domain - use at your own risk


; graphics environment manager canvas class
(define gem-canvas%
  (class canvas%
    (inherit get-parent
             get-dc
             suspend-flush
             resume-flush)
    (init-field frame-buf width height)
    
    (define/override (on-char key)
      (send (get-parent) on-char key))
    
    (super-new
     [min-width  width]
     [min-height height])

    (define bitmap
      (make-object bitmap% width height))

    (define dc (get-dc))
    
    (define/override (on-paint)
      (send bitmap
            set-argb-pixels
            0 0 width height
            frame-buf)
      
      (suspend-flush)
      (send dc clear)
      (send dc draw-bitmap bitmap 0 0)
      (resume-flush))))


; graphics environment manager class
(define gem%
  (class frame%
    (super-new)
    (init width height)
    (inherit show)
    
    (define frame-buf (make-bytes (* width height 4) 0))
    
    (define gl (new gem-canvas% 
                    [parent    this]
                    [frame-buf frame-buf]
                    [width     width]
                    [height    height]))
    
    (define exit? #f)
    
    (define/public (on-char key)
      (when (equal? (send key get-key-code) #\q)
        (set! exit? #t)
        (show #f)))
    
    ; repeatedly display frames until user quits
    (define/public (run renderer private-renderer-data)
      (define (run-loop)
        (unless exit?
          (time
           (renderer private-renderer-data frame-buf)
           (send gl on-paint)
           (queue-callback run-loop #f))))
      (show #t)
      (run-loop))))

; the block world map is stored in a cube of side mapdim; each map entry
; determines the colour of the corresponding block
(define MAPPOW 6)
(define MAPDIM (expt 2 MAPPOW))
(define MAPMASK (- MAPDIM 1))

; these are the image dimentions used in Minecraft4k
(define WIDTH 428)
(define HEIGHT 240)
(define SCALE 2)


; return map index of block at given (integer) co-ordinates
(define (mapindex x y z)
  (bitwise-ior
   (arithmetic-shift (bitwise-and z MAPMASK) (* MAPPOW 2))
   (arithmetic-shift (bitwise-and y MAPMASK) MAPPOW)
   (bitwise-and x MAPMASK)))

; return map index of block at given (real) co-ordinates
(define (mapindexf x y z)
  (mapindex (inexact->exact (floor x))
            (inexact->exact (floor y))
            (inexact->exact (floor z))))


; return frame buffer index of pixel at given co-ordinates
; (0, 0) is top left
(define (framebufindex x y)
  (* (+ (* WIDTH y) x) 4))

; set pixel at (x,y) in given gl-ubyte-vector 'buf' to given colour
(define (set-pixel-rgb! buf x y r g b)
  (let ([index (framebufindex x y)])
    (for ([i (in-range index (+ index 4))]
          [c (in-list (list #xFF r g b))])
      (bytes-set! buf i c))))

; create the world map
(define (generate-map)
  (for*/vector #:length (* MAPDIM MAPDIM MAPDIM) ([x MAPDIM] [y MAPDIM] [z MAPDIM])
    (define yd (* (- y 32.5) 0.4))
    (define zd (* (- z 32.5) 0.4))
    (if (or (> (random) (- (sqrt (sqrt (+ (* yd yd) (* zd zd)))) 0.8))
            (< (random) 0.6))
        0
        (random #x00FFFFFF))))

(define cl 0.0) ; use instead of a clock for now

; render the next frame of the 'blkmap' world into the given 'frame-buf'
(define (render-blocks blkmap frame-buf)
  (define dx cl)
  (define ox (+ 32.5 (* dx MAPDIM)))
  (define oy 32.5)
  (define oz 32.5)
  (set! cl (+ cl 0.01))
  
  (for ([x WIDTH])
    (define rotzd (/ (- x (/ WIDTH 2.0)) HEIGHT))
    (for ([y HEIGHT])
      (define rotyd (/ (- y (/ HEIGHT 2.0)) HEIGHT))
      (define rotxd 1.0)
      (define col 0)
      (define br 255)
      (define ddist 0.0)
      (define closest 32.0)
      (for ([d 3])
        (define dimLength 0.0)
        
        (cond
          [(= d 0) (set! dimLength rotxd)]
          [(= d 1) (set! dimLength rotyd)]
          [(= d 2) (set! dimLength rotzd)])
        
        (define ll (/ 1.0 (abs dimLength)))
        (define xd (* rotxd ll))
        (define yd (* rotyd ll))
        (define zd (* rotzd ll))
        (define initial 0.0)
        
        (cond
          [(= d 0) (set! initial (- ox (floor ox)))]
          [(= d 1) (set! initial (- oy (floor oy)))]
          [(= d 2) (set! initial (- oz (floor oz)))])
        
        (when (> dimLength 0)
          (set! initial (- 1 initial)))
        
        (define dist (* ll initial))
        (define xp (+ ox (* xd initial)))
        (define yp (+ oy (* yd initial)))
        (define zp (+ oz (* zd initial)))
        
        (when (< dimLength 0)
          (cond
            [(= d 0) (set! xp (- xp 1))]
            [(= d 1) (set! yp (- yp 1))]
            [(= d 2) (set! zp (- zp 1))]))
        
        (let loop ()
          (when (< dist closest)
            (define tex (vector-ref blkmap (mapindexf xp yp zp)))
            (when (> tex 0)
              (set! col tex)
              (set! ddist (- 255.0 (floor (* (/ dist 32.0) 255.0))))
              (set! br (/ (* 255 (- 255 (* 50 (modulo (+ d 2) 3)))) 255))
              (set! closest dist))
            (set! xp (+ xp xd))
            (set! yp (+ yp yd))
            (set! zp (+ zp zd))
            (set! dist (+ dist ll))
            (loop)))
        
        (define r (inexact->exact 
                   (floor 
                    (/ (* (bitwise-and (arithmetic-shift col -16) #xFF) br ddist) 
                       (* 255.0 255)))))
        (define g (inexact->exact 
                   (floor 
                    (/ (* (bitwise-and (arithmetic-shift col -8) #xFF) br ddist) 
                       (* 255.0 255))))) 
        (define b (inexact->exact 
                   (floor 
                    (/ (* (bitwise-and col #xFF) br ddist)
                       (* 255.0 255))))) 
        (set-pixel-rgb! frame-buf x y r g b)))))


(define (main)
  (let ([blkmap (generate-map)]
        [graphics (new gem% 
                       [width  WIDTH] 
                       [height HEIGHT]
                       [label  "mc4k - press q to exit"])])
    (send graphics run render-blocks blkmap)))

;(main)
