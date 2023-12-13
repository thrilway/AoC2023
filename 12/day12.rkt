#lang racket
(require memo)

(define/memoize (arrangements springs groups run-length)
  (match  (cons springs groups)
    ((cons (list #\# next ...) _)
     (if
        (or
          (null? groups)
          (>= run-length (car groups)))
        0
        (arrangements next groups (add1 run-length))))
    ((cons (list #\. next ...) _)
     (cond 
       ((zero? run-length)
        (arrangements next groups 0))
       ((= run-length (car groups))
        (arrangements next (cdr groups) 0))
       (else 0)))
    ((cons (list #\? next ...) _)
     (+ (arrangements (cons #\# next) groups run-length)
        (arrangements (cons #\. next) groups run-length)))
    ((cons _ (list g))
     (if (= g run-length) 1 0))
    ((cons _ '())
     (if (zero? run-length) 1 0))
    (_ 0)))

(define (parse-input in_p)
  (define (parse-line line)
    (let ((split (string-split line " ")))
     (cons
       (string->list (car split))
       (map string->number (string-split (cadr split) ",")))))
  (let loop ((cur (read-line in_p)) (acc '()))
   (if (eof-object? cur)
       acc
       (loop (read-line in_p) (append acc (list (parse-line cur)))))))

(define (expand-row row n)
  (define (expand-springs)
    (foldl (lambda (x acc) (append acc (list #\?) x)) (car row) (make-list (sub1 n) (car row))))
  (define (expand-groups)
    (apply append (make-list n (cdr row))))
  (cons (expand-springs) (expand-groups)))

(define (part1 in_p)
  (define data (parse-input in_p))
  (for/fold
    ((out 0))
    ((row data))
    (+ out (arrangements (car row) (cdr row) 0))))

(define (part2 in_p)
  (define data (map (lambda (r) (expand-row r 5)) (parse-input in_p)))
  (for/fold
    ((out 0))
    ((row data))
    (+ out (arrangements (car row) (cdr row) 0))))


(require rackunit)
(define test-input #<<HERE
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
HERE
  )

(check-equal? (part1 (open-input-string test-input)) 21)
(check-equal? (part2 (open-input-string test-input)) 525152)
