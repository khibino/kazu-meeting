(define (filter f l)
  (if (null? l)
      '()
      (let ((e (car l))
            (rest (filter f (cdr l))))
        (if (f e) (cons e rest) rest))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row _ rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (letrec ((p (car positions))
           (safe-one?
            (lambda (kk pp)
              (not (or (= k kk)
                       (= p pp)
                       (= (abs (- k kk))
                          (abs (- p pp)))))))
           (safe-rec?
            (lambda (kk rest)
              (if (null? rest)
                #t
                (and (safe-one? kk (car rest))
                     (safe-rec? (- kk 1) (cdr rest)))))))
    (safe-rec? (- k 1) (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (bad-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
