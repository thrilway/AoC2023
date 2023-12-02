#lang racket
(require racket/port)

(define test-data #<<HERE
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
HERE
  )
(define max/hash
  (hash "red" 12
        "green" 13
        "blue" 14))
(define/contract (parse-draws draws)
    (-> string? (hash/c string? integer?))
    (let ((draw-strings (string-split draws ";")))
     (let ((draw-hashes (map parse-draw draw-strings)))
      (for*/fold ((cur (car draw-hashes))
                  #:result cur)
        ((h (cdr draw-hashes))
         ((k v) (in-hash h)))
        (let ((prev (hash-ref cur k 0)))
         (if (> v prev)
             (hash-set cur k v)
             cur))))))

(define/contract (parse-draw draw)
    (-> string? hash?)
    (for*/fold ((out (hash)))
      ((s (string-split draw ","))
       )
      (let ((l (string-split s " ")))
       (hash-set out (second l) (string->number (car l))))))
(define/contract (parse-game line)
  (-> string? (values integer? hash?))
  (define rgx #px"^Game (\\d*):(.*)$")  
  (let ((m (regexp-match rgx line)))
   (values (string->number (second m))
           (parse-draws (third m)))))
(define/contract (power-of game)
  (-> hash? integer?)
  (for/fold ((out 1) #:result out)
    ((v (in-hash-values game)))
    (* out v)))


(define/contract (possible-game? game)
  (-> hash? boolean?)
  (for/and 
    (((k v) (in-hash game)))
    (or (not (hash-has-key? max/hash k))
        (<= v (hash-ref max/hash k)))))

(define/contract (part1 in_p)
  (-> port? integer?)
  (for/fold ((out 0)
             #:result out
             )
    ((line (in-lines in_p)))
    (let-values (((idx game) (parse-game line)))
      (if (possible-game? game)
          (+ out idx)
          out))))
(define/contract (part2 in_p)
  (-> port? integer?)
  (for/fold ((out 0) #:result out)
    ((line (in-lines in_p)))
    (let-values (((_ game) (parse-game line)))
      (+ out (power-of game)))))
(require rackunit)
(check-equal? (call-with-input-string test-data part1)
              8)
(check-equal? (call-with-input-string test-data part2)
              2286)
