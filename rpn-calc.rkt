#lang racket

(require racket/gui/base)
(require data/gvector)

(define calc "")
(define nums (make-gvector))

(define (update-calc)
  (set! calc "")
  (for/gvector ([e nums])
    (set! calc (string-append calc e ", "))))

(define frame (new frame%
                   [label "Calc"]
                   [width 300]
                   [height 500]
                   [x 200]
                   [y 300]))

(define v-panel (new vertical-panel% [parent frame]))

(define msg (new message%
                 [parent v-panel]
                 [auto-resize #t]
                 [label "nothing pressed yet"]))

(define h-panels (make-vector 5 horizontal-panel%))

; maybe make a growable vector and use for?
(vector-set! h-panels 0 (new horizontal-panel% [parent v-panel]))
(vector-set! h-panels 1 (new horizontal-panel% [parent v-panel]))
(vector-set! h-panels 2 (new horizontal-panel% [parent v-panel]))
(vector-set! h-panels 3 (new horizontal-panel% [parent v-panel]))
(vector-set! h-panels 4 (new horizontal-panel% [parent v-panel]))

(for ([y (in-inclusive-range 1 3)])
  (for ([x (in-inclusive-range 1 3)])
    (new button%
         [parent (vector-ref h-panels (- y 1))]
         [label (number->string (+ x (* (- y 1) 3)))]
         [stretchable-width #t]
         [stretchable-height #t]
         [callback (lambda (button event)
                     (gvector-add! nums (number->string (+ x (* (- y 1) 3))))
                     (update-calc)
                     (send msg set-label calc))])))
(new button%
     [parent (vector-ref h-panels 3)]
     [label "+"]
     [stretchable-width #t]
     [stretchable-height #t]
     [callback (lambda (button event)
                 (cond
                   [(> (gvector-count nums) 1) (gvector-add! nums (number->string (+ (string->number (gvector-remove-last! nums)) (string->number (gvector-remove-last! nums)))))])
                 (update-calc)
                 (send msg set-label calc))])
(new button%
     [parent (vector-ref h-panels 3)]
     [label "-"]
     [stretchable-width #t]
     [stretchable-height #t]
     [callback (lambda (button event)
                 (cond
                   [(> (gvector-count nums) 1) (let ([n1 (string->number (gvector-remove-last! nums))] [n2 (string->number (gvector-remove-last! nums))]) (gvector-add! nums (number->string (- n2 n1))))])
                 (update-calc)
                 (send msg set-label calc))])
(new button%
     [parent (vector-ref h-panels 3)]
     [label "*"]
     [stretchable-width #t]
     [stretchable-height #t]
     [callback (lambda (button event)
                 (cond
                   [(> (gvector-count nums) 1) (gvector-add! nums (number->string (* (string->number (gvector-remove-last! nums)) (string->number (gvector-remove-last! nums)))))])
                 (update-calc)
                 (send msg set-label calc))])
(new button%
     [parent (vector-ref h-panels 3)]
     [label "/"]
     [stretchable-width #t]
     [stretchable-height #t]
     [callback (lambda (button event)
                 (cond
                   [(> (gvector-count nums) 1) (let ([n1 (string->number (gvector-remove-last! nums))] [n2 (string->number (gvector-remove-last! nums))]) (gvector-add! nums (number->string (/ n2 n1))))])
                 (update-calc)
                 (send msg set-label calc))])

(new button%
     [parent (vector-ref h-panels 4)]
     [label "c"]
     [stretchable-width #t]
     [stretchable-height #t]
     [callback (lambda (button event)
                 (for ([x (in-range 0 (gvector-count nums))])
                   (gvector-remove-last! nums))
                 (send msg set-label ""))])

(send frame show #t)
