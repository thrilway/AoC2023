#lang racket

(struct/contract grid ((vals (vectorof char?)) (width integer?) (height integer?)))
(define/contract (grid-ref g coord)
  (-> grid? (cons/c integer? integer?) any/c)
  (vector-ref (grid-vals g) (+ (car coord) (* (cdr coord) (grid-width g)))))
(define/contract (grid-split-vert g col)
  (-> grid? integer? (values grid? grid?))
  (let loop ((rem (grid-vals g)) (l-acc (vector)) (r-acc (vector)))
   (if (vector-empty? rem)
       (values (grid l-acc col (grid-height g))
               (grid r-acc (- (grid-width g) col) (grid-height g)))
       (let-values (((cur next) (vector-split-at rem (grid-width g))))
         (call-with-values (lambda () (vector-split-at cur col)) (lambda (l r) (loop next (vector-append l-acc l) (vector-append r-acc r))))))))
(define/contract (grid-split-horiz g row)
  (-> grid? integer? (values grid? grid?))
  (let-values (((top bottom) (vector-split-at (grid-vals g) (* (grid-width g) row))))
    (values (grid top (grid-width g) row)
            (grid bottom (grid-width g) (- (grid-height g) row)))))

(define/contract (grid-flip-vert g)
  (-> grid? grid?)
  (let loop ((rem (grid-vals g)) (acc (vector)))
   (if (vector-empty? rem)
       (struct-copy grid g (vals acc))
       (let-values (((cur next) (vector-split-at rem (grid-width g))))
         (loop next (vector-append cur acc))))))
(define/contract (grid-flip-horiz g)
  (-> grid? grid?)
  (let loop ((rem (grid-vals g)) (acc (vector)))
   (if (vector-empty? rem)
       (struct-copy grid g (vals acc))
       (let-values (((cur next) (vector-split-at rem (grid-width g))))
         (loop next (vector-append acc (list->vector (reverse (vector->list cur)))))))))
(define/contract (grid-mirror-at-col? g col)
  (-> grid? integer? boolean?)
  (define-values (tmp-r tmp-l) (grid-split-vert g col))
  (define-values (l lg) (grid-split-vert tmp-l (min (grid-width tmp-l) (grid-width tmp-r))))
  (define-values (r rg) (grid-split-vert (grid-flip-horiz tmp-r) (min (grid-width tmp-l) (grid-width tmp-r))))
  (equal? (grid-vals l) (grid-vals r)))
(define/contract (grid-mirror-at-row? g row)
  (-> grid? integer? boolean?)
  (define-values (tmp-t tmp-b) (grid-split-horiz g row))
  (define-values (t tg) (grid-split-horiz (grid-flip-vert tmp-t) (min (grid-height tmp-t) (grid-height tmp-b))))
  (define-values (b bg) (grid-split-horiz tmp-b (min (grid-height tmp-t) (grid-height tmp-b))))
  (equal? (grid-vals t) (grid-vals b)))  

(define/contract (smudged-grid-mirror-at-col? g col)
  (-> grid? integer? boolean?)
  (define-values (tmp-r tmp-l) (grid-split-vert g col))
  (define-values (l lg) (grid-split-vert tmp-l (min (grid-width tmp-l) (grid-width tmp-r))))
  (define-values (r rg) (grid-split-vert (grid-flip-horiz tmp-r) (min (grid-width tmp-l) (grid-width tmp-r))))
  (let loop ((idx 0) (smudges 0))
   (cond
     ((and (= idx (vector-length (grid-vals l))) (= 1 smudges)) #t)
     ((= idx (vector-length (grid-vals l))) #f)
     ((> smudges 1) #f)
     ((char=? (vector-ref (grid-vals l) idx) (vector-ref (grid-vals r) idx)) (loop (add1 idx) smudges))
     (else (loop (add1 idx) (add1 smudges))))))
(define/contract (smudged-grid-mirror-at-row? g row)
  (-> grid? integer? boolean?)
  (define-values (tmp-t tmp-b) (grid-split-horiz g row))
  (define-values (t tg) (grid-split-horiz (grid-flip-vert tmp-t) (min (grid-height tmp-t) (grid-height tmp-b))))
  (define-values (b bg) (grid-split-horiz tmp-b (min (grid-height tmp-t) (grid-height tmp-b))))
  (let loop ((idx 0) (smudges 0))
   (cond
     ((and (= idx (vector-length (grid-vals t))) (= 1 smudges)) #t)
     ((= idx (vector-length (grid-vals t))) #f)
     ((> smudges 1) #f)
     ((char=? (vector-ref (grid-vals t) idx) (vector-ref (grid-vals b) idx)) (loop (add1 idx) smudges))
     (else (loop (add1 idx) (add1 smudges))))))
(define/contract (draw-grid g)
  (-> grid? void?)
  (for/fold
    ((out "")
     #:result (display (string-trim out)))
    ((ch (in-vector (grid-vals g)))
     (i (in-naturals)))
    (if (zero? (remainder i (grid-width g)))
        (string-append out (string #\newline ch))
        (string-append out (string ch)))))
(define/contract (parse-input in_p)
  (-> port? (listof grid?))
  (let loop ((cur (read-line in_p)) (width 0) (height 0) (cur-acc (vector)) (acc '()))
   (cond 
     ((eof-object? cur) (append acc (list (grid cur-acc width height))))
     ((string=? cur "") (loop (read-line in_p) 0 0 (vector) (append acc (list (grid cur-acc width height)))))
     (else (loop (read-line in_p) 
                 (string-length cur) 
                 (add1 height) 
                 (vector-append cur-acc (list->vector (string->list cur)))
                 acc)))))
(define/contract (summary g)
  (-> grid? integer?)
  (or
   (for/or
    ((col (in-range 1 (grid-width g))))
     (if (grid-mirror-at-col? g col)
         col
         #f))
   (for/or
       ((row (in-range 1 (grid-height g))))
     (if (grid-mirror-at-row? g row)
         (* 100 row)
         #f))))
(define/contract (smudge-summary g)
  (-> grid? integer?)
  (or
   (for/or
    ((col (in-range 1 (grid-width g))))
     (if (smudged-grid-mirror-at-col? g col)
         col
         #f))
   (for/or
       ((row (in-range 1 (grid-height g))))
     (if (smudged-grid-mirror-at-row? g row)
         (* 100 row)
         #f))))
(define/contract (part1 in_p)
  (-> port? integer?)
  (let ((data (parse-input in_p)))
   (for/fold
     ((out 0))
     ((grd data))
     (+ out (summary grd)))))
(define/contract (part2 in_p)
  (-> port? integer?)
  (let ((data (parse-input in_p)))
   (for/fold
     ((out 0))
     ((grd data))
     (+ out (smudge-summary grd)))))

(require rackunit)

(define test-input #<<HERE
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
HERE
  )

(check-equal? (part1 (open-input-string test-input)) 405)
(check-equal? (part2 (open-input-string test-input)) 400)

