#lang racket

(define test-data #<<HERE
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
HERE
  )

(define/contract (parse-input in_p)
  (-> port? (listof (cons/c (vectorof integer?) (vectorof integer?))))
  (for/fold ((out '())
             #:result out)
            ((line (in-lines in_p)))
    (append out (list (parse-line (string-normalize-spaces line))))))
(define/contract (parse-line line)
  (-> string? (cons/c (vectorof integer?) (vectorof integer?)))
  (let*-values (((_ nums) (apply values (string-split line ":")))
                ((left right) (apply values (string-split nums "|"))))
    (let ((lnums (map (compose string->number string-trim) (string-split left " ")))
          (rnums (map (compose string->number string-trim) (string-split right " "))))
      (cons (list->vector lnums) (list->vector rnums)))))

(define/contract (part1 in_p)
  (-> port? integer?)
  (define/contract (process-row row)
    (-> (cons/c (vectorof integer?) (vectorof integer?)) integer?)
    (for/fold ((out 0) #:result (if (zero? out) 0 (expt 2 (sub1 out))))
               ((n (in-vector (car row))))
      (if (vector-member n (cdr row))
          (add1 out)
          out)))
  (let ((rows (parse-input in_p)))
    (for/fold ((out 0) #:result out)
              ((row rows))
      (+ out (process-row row)))))

(define/contract (part2 in_p)
  (-> port? integer?)
  (define rows (parse-input in_p))
  (define/contract (process-row row)
    (-> (cons/c (vectorof integer?) (vectorof integer?)) integer?)
    (for/fold ((out 0) #:result out)
      ((n (in-vector (car row))))
      (if (vector-member n (cdr row))
          (add1 out)
          out)))
  (define row-results
    (map process-row rows))
  (define/contract (extend-res cur-res idx n)
    (-> (listof integer?) integer? integer? (listof integer?))
    (for/fold ((out cur-res) #:result out)
      ((j (in-range (add1 idx) (+ (add1 idx) n))))
      (if (= j (length out))
          (append out (list 1))
          (list-set out j (+ (list-ref out j) (list-ref out idx))))))
  (let loop ((idx 0) (acc (make-list (length rows) 1)))
   (if (>= idx (length acc))
       (apply + acc)
       (let ((more (list-ref row-results idx)))
        (if (zero? more)
            (loop (add1 idx) acc)
            (loop (add1 idx) (extend-res acc idx more)))))))

(require rackunit)
(check-equal? (call-with-input-string test-data part1) 13)
(check-equal? (call-with-input-string test-data part2) 30)
              
