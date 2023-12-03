#lang racket

(define test-data #<<HERE
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
HERE
  )

(define (parse-input in_p)
  (let loop ((cur (read-char in_p))
             (col 0)
             (row 0)
             (cur-num '())
             (cur-num-coords '())
             (sym-coords '())
             (num-acc '()))
    (cond ((and
            (char? cur)
            (not (char-numeric? cur))
            (not (null? cur-num)))
           (loop cur col row '() '() sym-coords (cons (cons (string->number (apply string cur-num)) cur-num-coords) num-acc)))
          ((eof-object? cur)
           (values num-acc (list->vector sym-coords)))
          ((char=? cur #\newline)
           (loop (read-char in_p) 0 (add1 row) '() '() sym-coords num-acc))
          ((char=? cur #\.)
           (loop (read-char in_p) (add1 col) row '() '() sym-coords num-acc))
          ((char-numeric? cur)
           (loop (read-char in_p) (add1 col) row (append cur-num (list cur)) (append cur-num-coords (list (cons row col))) sym-coords num-acc))
          (else (loop (read-char in_p) (add1 col) row '() '() (append sym-coords (list (cons row col))) num-acc)))))

(define (symbol-adjacent? num/coords sym-coords)
  (define (gen-adjacents coord)
    (list (cons (add1 (car coord)) (add1 (cdr coord)))
          (cons (add1 (car coord)) (cdr coord))
          (cons (add1 (car coord)) (sub1 (cdr coord)))
          (cons (car coord) (add1 (cdr coord)))
          (cons (car coord) (sub1 (cdr coord)))
          (cons (sub1 (car coord)) (add1 (cdr coord)))
          (cons (sub1 (car coord)) (cdr coord))
          (cons (sub1 (car coord)) (sub1 (cdr coord)))))
  (for*/or
      ((num/coord (cdr num/coords))
       (coord (gen-adjacents num/coord)))
    (if (vector-member coord sym-coords)
        #t
        #f)))

(define (part1 in_p)
  (define-values (num/coords sym-coords) (parse-input in_p))
  (for/fold ((out 0) #:result out)
            ((n/c num/coords))
    (if (symbol-adjacent? n/c sym-coords)
        (+ out (car n/c))
        out)))

(require rackunit)
(check-equal? (call-with-input-string test-data part1) 4361)
        