

(require racket/draw)
(define target (make-bitmap 800 800))
(define dc (new bitmap-dc% [bitmap target]))

(send dc draw-rectangle 10 10 0 0)
(send dc draw-rectangle 110 10 79 19)
(send dc draw-rectangle 210 10 0 0)
(send dc draw-rectangle 310 10 0 0)
(send dc draw-rectangle 410 10 78 19)

(send dc draw-rectangle 10 110 19 19)
(send dc draw-rectangle 110 110 19 19)
(send dc draw-rectangle 210 110 19 49)
(send dc draw-rectangle 310 110 19 79)
(send dc draw-rectangle 410 110 19 79)

(send dc draw-rectangle 10 210 49 49)
(send dc draw-rectangle 110 210 49 19)
(send dc draw-rectangle 210 210 49 49)
(send dc draw-rectangle 310 210 0 0)
(send dc draw-rectangle 410 210 49 49)

(send dc draw-rectangle 10 310 0 0)
(send dc draw-rectangle 110 310 79 19)
(send dc draw-rectangle 210 310 0 0)
(send dc draw-rectangle 310 310 0 0)
(send dc draw-rectangle 410 310 78 19)

(send dc draw-rectangle 10 410 49 49)
(send dc draw-rectangle 110 410 79 19)
(send dc draw-rectangle 210 410 49 49)
(send dc draw-rectangle 310 410 19 78)
(send dc draw-rectangle 410 410 49 49)

(send target save-file "ex.png" 'png)
