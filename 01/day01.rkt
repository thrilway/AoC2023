#lang racket
(require racket/port)

(define test-data #<<HERE
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
HERE
  )
(define test-data2 
  #<<HERE
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
HERE
)


(define/contract (part1 in_port)
  (-> port? integer?)
  (define/contract (parse-line line)
    (-> string? integer?)
    (let ((m1 (regexp-match #px"^\\D*(\\d).*$" line))
          (m2 (regexp-match #px"^.*(\\d)\\D*$" line)))
     (string->number (string-append (second m1) (second m2)) 10)))
  (for/fold ((out 0) #:result out)
    ((line (in-lines in_port)))
    (+ out (parse-line line))))

(define/contract (part2 in_port)
  (-> port? integer?)
  (define digits
    (hash "one" "1"
          "two" "2"
          "three" "3"
          "four" "4"
          "five" "5"
          "six" "6"
          "seven" "7"
          "eight" "8"
          "nine" "9"))
  (define digit-regexp/string
    (string-append
      "("
      (string-join (hash-keys digits) "|")
      "|\\d)"
      ))
  (define/contract (digit? s)
    (-> string? boolean?)
    (if (regexp-match #px"^\\d$" s)
        #t
        #f))
  (define/contract (parse-line line)
    (-> string? integer?)
    (let ((m1 (regexp-match (pregexp (format "^.*?~a.*$" digit-regexp/string)) line))
          (m2 (regexp-match (pregexp (format "^.*~a.*?$" digit-regexp/string)) line)))
      (match (cons (second m1) (second m2))
        ((cons (? digit? d1)  (? digit? d2)) 
         (string->number (string-append d1 d2)))
        ((cons (? digit? d1) s2)
         (string->number (string-append d1 (hash-ref digits s2))))
        ((cons s1 (? digit? d2))
         (string->number (string-append (hash-ref digits s1) d2)))
        ((cons s1 s2) (string->number (string-append (hash-ref digits s1) (hash-ref digits s2)))))))
  (for/fold ((out 0)
             #:result out)
    ((line (in-lines in_port)))
    (+ out 
       (parse-line line))))

(require rackunit)
(check-equal? (call-with-input-string test-data part1) 142)
(check-equal?(call-with-input-string test-data2 part2) 281)
       
