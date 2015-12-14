#lang racket

(struct user (id name [friends #:auto #:mutable])
  #:auto-value '())

(define users (list (user 0 "Hero")
                    (user 1 "Dunn")
                    (user 2 "Sue")
                    (user 3 "Chi")
                    (user 4 "Thor")
                    (user 5 "Clive")
                    (user 6 "Hicks")
                    (user 7 "Devin")
                    (user 8 "Kate")
                    (user 9 "Klein")))

(define friendships '((0 1) (0 2) (1 2) (1 3) (2 3)
                            (3 4) (4 5) (5 6) (5 7)
                            (6 8) (7 8) (8 9)))

(define (add-friend user friend)
  (unless (member friend (user-friends user))
    (set-user-friends! user (cons friend (user-friends user)))))

(define (find-user users id)
  (cond [(empty? users) #f]
        [(= (user-id (first users)) id) (first users)]
        [else (find-user (rest users) id)]))

(define (connect-users users friendships)
  (cond [(empty? friendships) users]
        [else
         (define friendship (first friendships))
         (define friend1 (find-user users (first friendship)))
         (define friend2 (find-user users (second friendship)))
         (add-friend friend1 friend2)
         (add-friend friend2 friend1)
         (connect-users users (rest friendships))]))

(define (number-of-friends user)
  (length (user-friends user)))

(define (sort-by-friends users)
  (sort users > #:key number-of-friends))

(connect-users users friendships)

(define (intersection lst1 lst2)
  (define (intersection-acc lst1 lst2 acc)
    (cond [(or (empty? lst1) (empty? lst2)) acc]
          [(member (first lst1) lst2)
           (intersection-acc (rest lst1) lst2 (cons (first lst1) acc))]
          [else
           (intersection-acc (rest lst1) lst2 acc)]))
  (intersection-acc lst1 lst2 '()))

(define (mutual-friends user1 user2)   
  (intersection (user-friends user1) (user-friends user2)))

(define (mutual-friend-count user1 user2)
  (length (mutual-friends user1 user2)))