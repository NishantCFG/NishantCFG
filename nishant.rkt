#lang racket
 
 
(define generate
  (lambda (cfg)
    (let ((rules (first (rest cfg))))
    (flatten
    (cond
      [(not (terminal? (first cfg) rules)) (apply append (list(map (lambda (y) (generate (list y rules))) (first (rest (pick (filter (lambda (x) (equal? (first cfg) (first x))) rules)))))))]
      [(terminal? (first cfg) rules) (first cfg)]
    )
   ))
 )
)
 
(define (terminal? e rules)
  (if (null? rules)
      #t
      (if (equal? e (first (first rules)))
          #f
          (terminal? e (rest rules))
      )
  )
)
 
 
(define pick
  (lambda (lst)
    (cond
      [(null? lst) #f]
      [else (list-ref lst (random (length lst)))]
      )
    )
  )
   ; (error "pick isn't defined yet!")))
 
(define pick-string
  (lambda (exp)
    (pick-string-helper exp)
    )
  )
 
    ;(error "pick-string isn't defined yet")))
(define (pick-string-helper exp) ;still doesnt work
  (cond
      [(null? exp) `()]
      [(re-string? exp) (pull-elements exp)]
      [(re-concatenation? exp) (pull-elements(map pick-string-helper (rest exp)) )] ;concatenation puts all of the ones together
      [(re-union? exp) (pull-elements (pick-string-helper (list-ref exp (+ (random (- (length exp) 1 )) 1))) )] ;union picks one of the things at random
      [(re-star? exp) (pull-elements (star-generator exp (random 3)) ) ]
      )
  )
 
(define (pull-elements exp)
  (cond
    [(null? exp) `()]
    [(list? (first exp)) (append (pull-elements (first exp)) (pull-elements (rest exp)))]
    [else (append (list (first exp)) (pull-elements (rest exp)))]
    )
  )
(define (star-generator exp rand)
  (cond
    [(= rand 0) `()] ;case to generate the mepty list. should this be doing something else?
    [(= rand 1) (append (map first (map pick-string-helper (rest exp))) (star-generator exp (random 3)))]
    [(= rand 2) (pick-string-helper exp)]
    )
  )
 
(define re-string?
  (lambda (exp)
    (cond
      [(null? exp) #t]
      [(or (equal? (first exp) `+) (or (equal? (first exp) `!) (equal? (first exp) `*))) #f]
      [else #t]
      )
    )
  )
   ; (error "re-string? isn't defined yet!")))
 
(define re-concatenation?
  (lambda (exp)
    (cond
      [(null? exp) #f]
      [(equal? (first exp) `!) #t]
      [else #f]
      )
    )
  )
   ; (error "re-concatenation? isn't defined yet!")))
 
(define re-union?
  (lambda (exp)
    (cond
      [(null? exp) #f]
      [(equal? (first exp) `+) #t]
      [else #f]
      )
    )
  )
  ; (error  "re-union? isn't defined yet!")))
 
(define re-star?
  (lambda (exp)
    (cond
      [(null? exp) #f]
      [(equal? (first exp) `*) #t]
      [else #f]
      )
    )
  )
   ; (error "re-star? isn't defined yet!")))
 
(define op
  (lambda (exp)
    (cond
      [(re-string? exp) `error]
      [else (first exp)]
      )
    )
  )
   ; (error "op isn't defined yet!")))
 
(define args
  (lambda (exp)
    (args-assist (rest exp))
    )
  )
    ;(error  "args isn't defined yet!")))
 
(define (args-assist exp)
  (cond
    [(null? exp) `()]
    [else (append (list (first exp)) (args-assist (rest exp)))]
    )
  )
   
(define arg
  (lambda (exp)
    (cond
      [(not (equal? (first exp) `*)) `error]
      [else (arg-assist (rest exp))]
      )
    )
  )
(define (arg-assist exp) ;returns an extra list
  (cond
     [(null? exp) `()]
     [else (append (first exp) (args-assist (rest exp)))]
      )
    )
 
 
(define rulen-1 `(<sentence> (<derogatory> <justkidding>))) ;self-deprecating
(define rulen-2 `(<sentence> (<quote> <justkidding>)))
(define rulen-3 `(<derogatory> (Fuck the Patriarchy)))
(define rulen-4 `(<sentence> (<question> <justkidding>)))
(define rulen-5 `(<derogatory> (I dont have a penis)))
(define rulen-6 `(<derogatory> (<asshole>)))
(define rulen-7 `(<asshole> (Im an asshole)))
(define rulen-8 `(<asshole> (On a scale of 1 to Nishant |,| how much of an asshole <specifier>)))
(define rulen-9 `(<specifier> (am I)))
(define rulen-10 `(<specifier> (are you)))
(define rulen-11 `(<specifier> (is <name>)))
(define rulen-12 `(<question> (On a scale of 1 to <name> how <adjective1> <specifier>)))
(define rulen-13 `(<sentence> (<action> <sentence> <justkidding>)))
(define rulen-14 `(<sentence> (<action> <sentence> <justkidding> <action2>)))
(define rulen-15 `(<action> (Walks into room <condition>)))
(define rulen-16 `(<action> (Stands up)))
(define rulen-17 `(<action> (Begs for hug)))
(define rulen-28 `(<action> (Walks into room)))
(define rulen-31 `(<action> (Plays music)))
(define rulen-18 `(<action2> (Walks out)))
(define rulen-19 `(<action2> (Sits down)))
(define rulen-20 `(<action2> (Waves sword)))
(define rulen-21 `(<action2> (Begs for hug)))
(define rulen-32 `(<action2> (Plays music)))
(define rulen-22 `(<derogatory> (Why do you hate me?)))
(define rulen-23 `(<derogatory> (I love it when you guys make fun of me)))
(define rulen-24 `(<derogatory> (Fuck you guys)))
(define rulen-25 `(<adjective1> (fucked up)))
(define rulen-26 `(<adjective1> (retarded)))
(define rulen-27 `(<adjective1> (stupid)))
(define rulen-33 `(<condition> (drunk)))
(define rulen-29 `(<condition> (bundled up)))
(define rulen-30 `(<condition> (at 6 A.M.)))
(define rulen-34 `(<quote> (Burn them all)))
(define rulen-35 `(<quote> (A Lannister always pays his debts)))
(define rulen-36 `(<quote> (Youre a Lannister. Youre my son)))
(define rulen-37 `(<derogatory> (Its because Im brown isnt it?)))
(define rulen-38 `(<justkidding> ( )))
(define rulen-39 `(<justkidding> ( )))
(define rulen-40 `(<justkidding> (... just kidding)))
(define rulen-80 `(<name> (<realname>)))
(define rulen-81 `(<name> (<nname>)))
(define rulen-83 `(<realname> (Dan)))
(define rulen-84 `(<realname> (Andrew)))
(define rulen-85 `(<realname> (Aaron)))
(define rulen-86 `(<realname> (Eli)))
(define rulen-87 `(<realname> (Ted)))
(define rulen-88 `(<realname> (Connor)))
(define rulen-89 `(<realname> (Shreyas)))
(define rulen-90 `(<realname> (Matt)))
(define rulen-91 `(<nname> (Aaron Targaryen)))
(define rulen-92 `(<nname> (<andrewnname>))) ;Either Andrew Maltus or Andrew Lannister
(define rulen-93 `(<nname> (Dread Lord)))
(define rulen-94 `(<nname> (Eli Martell)))
(define rulen-95 `(<nname> (Daniel Lannister)))
(define rulen-96 `(<nname> (Connor McCann of the House McCann)))
(define rulen-97 `(<nname> (Matthew Lannister)))
(define rulen-98 `(<nname> (Shreyas Krishnapuram Tirumala)))
(define rulen-99 `(<andrewname> (Andrew Maltus)))
(define rulen-100 `(<andrewname> (Andrew Lannister)))
 
 
(define cfgn
  (list `<sentence> (list rulen-1 rulen-2 rulen-3 rulen-4 rulen-5 rulen-6 rulen-7 rulen-8 rulen-9 rulen-10 rulen-11 rulen-12
                         rulen-13 rulen-14 rulen-15 rulen-16 rulen-17 rulen-18 rulen-19 rulen-20 rulen-21 rulen-22 rulen-23 rulen-24
                         rulen-26 rulen-27 rulen-28 rulen-29 rulen-30 rulen-31 rulen-32 rulen-33 rulen-34 rulen-35 rulen-36 rulen-37
                         rulen-38 rulen-39 rulen-40
                         
                         rulen-80 rulen-81 rulen-83 rulen-84 rulen-85 rulen-86 rulen-87 rulen-88 rulen-89 rulen-90
                         rulen-91 rulen-92 rulen-93 rulen-94 rulen-95 rulen-96 rulen-97 rulen-98 rulen-99 rulen-100
                         )
        )
  )