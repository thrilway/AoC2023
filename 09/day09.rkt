#lang racket

(define/contract (parse-input in_p)
  (-> port?  (listof (listof integer?)))
  (for/fold ((out '()))
    ((line (in-lines in_p)))
    (append out (list (map string->number (string-split line))))))


(define/contract (get-prediction nums/l dir)
  (-> (listof integer?) symbol? integer?)
  (let loop ((cur nums/l) (acc '()))
   (if (null? (filter (compose not zero?) cur))
       (match dir
         ('forward (apply + acc))
         ('back (foldl (lambda (r l) (- r l)) 0 acc)))       
       (let i-loop ((rem cur) (i-acc '()))
        (if (null? (cdr rem))
            (match dir
              ('forward (loop i-acc (cons (last cur) acc)))
              ('back (loop i-acc (cons (car cur) acc))))            
            (i-loop (cdr rem) (append i-acc (list (- (cadr rem) (car rem))))))))))
(define/contract (part1 in_p)
  (-> port? integer?)
  (let loop ((rem (parse-input in_p)) (acc 0))
   (if (null? rem)
       acc
       (loop (cdr rem) (+ acc (get-prediction (car rem) 'forward))))))

(define/contract (part2 in_p)
  (-> port? integer?)
  (let loop ((rem (parse-input in_p)) (acc 0))
   (if (null? rem)
       acc
       (loop (cdr rem) (+ acc (get-prediction (car rem) 'back))))))

(require rackunit)
(define test-input #<<HERE
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
HERE
  )
(check-equal? (part1 (open-input-string test-input)) 114)
(check-equal? (part2 (open-input-string test-input)) 2)
