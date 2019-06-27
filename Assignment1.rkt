#lang pl

;; QUESTION 1

;; Defining a test word
(define plPrefixWord "plol")

;; First Learning How String-ref Works
(test (string-ref plPrefixWord 0) => #\p)
(test (string-ref plPrefixWord 1) => #\l)


;; Finidnig the first character
(test (equal? (string-ref plPrefixWord 0) #\p) => true)
(test (and (equal? (string-ref plPrefixWord 0) #\p) (equal? (string-ref plPrefixWord 1) #\l)) => #t)

;; Defining the function
;; INPUT: takes 5 arguments of string, OUTPUT: boolean
;; RETURNS: the first string with prefix "pl", otherwise, false
(: plPrefixContained : String  String  String  String  String -> (U String Boolean))
(define (plPrefixContained s1 s2 s3 s4 s5)
  (cond
    [(and (equal? (string-ref s1 0) #\p) (equal? (string-ref s1 1) #\l)) s1]
    [(and (equal? (string-ref s2 0) #\p) (equal? (string-ref s2 1) #\l)) s2]
    [(and (equal? (string-ref s3 0) #\p) (equal? (string-ref s3 1) #\l)) s3]
    [(and (equal? (string-ref s4 0) #\p) (equal? (string-ref s4 1) #\l)) s4]
    [(and (equal? (string-ref s5 0) #\p) (equal? (string-ref s5 1) #\l)) s5]
    [else false]
    )
  )

(test (plPrefixContained "yyyt" "TplT" "plTT" "PlPl" "plplpl")  => "plTT")

(test (and (equal? (string-ref plPrefixWord 0) #\p) (equal? (string-ref plPrefixWord 1) #\l) plPrefixWord) => plPrefixWord)


;; QUESTION 2.a

(define shortWord "short")
(define longWord "veryVeryLongWord")

(test (string-length shortWord) => 5)
(test (< (string-length shortWord) (string-length longWord)) => true)

;
(define exampleList (list shortWord longWord 3 shortWord))

;
(test (null? exampleList) => false)
(test (string? (first (rest exampleList))) => true)

(test (cond [(string? (first exampleList)) (string-length (first exampleList))] [else false] ) => 5)



;;
(: longestString : (Listof Any) -> (U String Boolean));; Decliration
;;Definition
(define (longestString lst)
  (longestStringTailRecursion lst false))


;; recursion helper, takes the list and at first the value false indicating not finding any string
;; if strings were found, returns the longest of them
(: longestStringTailRecursion : (Listof Any) (U String Boolean) -> (U String Boolean)) ;;Decliration
;;Definition
(define (longestStringTailRecursion lst falseOrLongest)
  (cond
    ;; base condition
    [(null? lst) falseOrLongest]
    ;; one step
    [(and (string? (first lst)) ;; the first member is string
          (> (string-length (first lst)) #| if it is string returns the length of it, otherwise (boolean), return -1|#
             (if (string? falseOrLongest)(string-length falseOrLongest) -1)))
     #| Every one of the conditions were met (not false), returning the last value,
        which it's in this case a call of the same function with the rest of the list and the new longest string (tail recursion) |#
     (longestStringTailRecursion (rest lst) (first lst))]
    ;; the first meber isn't string
    [else (longestStringTailRecursion (rest lst) falseOrLongest)]
    )
  )


(test (longestString exampleList) => longWord)
(test (longestString '(34 uuu 90)) => false)
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")


;; QUESTION 2.b

;;
(: longestString : (Listof Any) -> (U String #f));; Decliration
;;Definition
(define (longestString lst)
  (longestStringTailRecursion lst false))


;; recursion helper, takes the list and at first the value false indicating not finding any string
;; if strings were found, returns the longest of them
(: longestStringTailRecursion : (Listof Any) (U String #f) -> (U String #f)) ;;Decliration
;;Definition
(define (longestStringTailRecursion lst falseOrLongest)
  (cond
    ;; base condition
    [(null? lst) falseOrLongest]
    ;; one step
    [(and (string? (first lst)) ;; the member is string
          (> (string-length (first lst)) #| if it is string returns the length of it, otherwise (boolean), return -1|#
             (if (string? falseOrLongest)(string-length falseOrLongest) -1)))
     #| Every one of the conditions were met (not false), returning the last value,
        which it's in this case a call of the same function with the rest of the list and the new longest string (tail recursion) |#
     (longestStringTailRecursion (rest lst) (first lst))]
    ;; contiune searching
    [else (longestStringTailRecursion (rest lst) falseOrLongest)]
    )
  )


;;
(: shortestString : (Listof Any) -> (U String #f));; Decliration
;;Definition
(define (shortestString lst)
  (shortestStringTailRecursion lst false))


;; recursion helper, takes the list and at first the value false indicating not finding any string
;; if strings were found, returns the longest of them
(: shortestStringTailRecursion : (Listof Any) (U String #f) -> (U String #f)) ;;Decliration
;;Definition
(define (shortestStringTailRecursion lst falseOrLongest)
  (cond
    ;; base condition
    [(null? lst) falseOrLongest]
    ;; one step
    [(and (string? (first lst)) ;; the first member is string
          (< (string-length (first lst)) #| if it is string returns the length of it, otherwise (boolean), return 2^16|#
             (if (string? falseOrLongest)(string-length falseOrLongest) (expt 2 16))))
     #| Every one of the conditions were met (not false), returning the last value,
        which it's in this case a call of the same function with the rest of the list and the new longest string (tail recursion) |#
     (shortestStringTailRecursion (rest lst) (first lst))]
    ;; the first meber isn't string
    [else (shortestStringTailRecursion (rest lst) falseOrLongest)]
    )
  )


;; Understadning how the help function should work
(test (list (shortestString '(any "Benny" 10 "OP" 8) ) (longestString '(any "Benny" 10 "OP" 8) )) => '("OP" "Benny"))


#| Help function for ""short&long-lists""
   INPUT: list, output:list
   RETURNS: the longest and shortest string in the list if any are presernt, otherwise, null
   OPERATES: using shortest to check if any string is present, if so,
     returns a list of shortest and longest string using shortestString & longestString respectively, otherwise, null
|#
(: short&longOfThelist  : (Listof Any) -> (Listof Any))
(define (short&longOfThelist lst)
  (cond
    [(false? (shortestString lst)) null]
    [else (list (shortestString lst) (longestString lst))]
    );(list (shortestString lst) (longestString lst))
  )


(test (short&longOfThelist '(any "Benny" 10 "OP" 8)) =>  '("OP" "Benny"))


;; INPUT: List of lists, OUTPUT: list of lists
;; RETURNS a list of lists where each member (list) mapped via short&longOfThelist
(: short&long-lists  : (Listof (Listof Any)) -> (Listof (Listof Any)))
(define (short&long-lists  lst)
  (map (lambda (x) (short&longOfThelist x)) lst)
  )
       

(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)) ("2 5 5" 1 "5gg" L) (v gggg "f")())) => '(("OP" "Benny") () ("5gg" "2 5 5") ("f" "f") ()))
(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))

;; QUESTION 3

(define-type KeyStack
  [EmptyKS] ;; Empty constructor
  [Push Symbol String KeyStack] ;; push constructor
  )

;; INPUT: Symbol and KeyStack, OUTPUT: String or false
;; RETURNS: The value (String) of the key if found, false otherwise
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack symbol stk)
  (cases stk
    [(EmptyKS) false]
    [(Push sym str keyStkRest) (if (equal? sym symbol) str (search-stack symbol keyStkRest))]
    )
  )

;; INPUT: KeyStack, OUTPUT: KeyStack or false
;; RETURNS: if not empty, removes the first element and returns rest of the stack, otherwise, false
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack stk)
  (cases stk
    [(EmptyKS) false]
    [(Push sym str keyStkRest) keyStkRest]
    )
  )


(test (EmptyKS) => (EmptyKS)) 
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) =>  
      (Push 'b "B" (Push 'a "A" (EmptyKS)))) 
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A"  (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) 
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA") 
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS)))) 
 
(test (pop-stack (EmptyKS)) => #f) 

;; QUESTION 4


(: is-odd? : Natural -> Boolean)
;; INPUT: Natural number, OUTPUT: Boolean
;; RETURNS: true if the given argument is odd natural number, otherwise, false
;; OPERATION: if the given number is zero returns false (Basic case), else returns the validation of x-1 is even 
(define (is-odd? x)
  (if (zero? x) false (is-even? (- x 1)))
  )

(: is-even? : Natural -> Boolean)
;; INPUT: Natural number, OUTPUT: Boolean
;; RETURNS: true if the given argument is even natural number, otherwise, false
;; OPERATION: if the given number is zero returns true (Basic case), else returns the validation of x-1 is odd 
(define (is-even? x)
  (if (zero? x) true (is-odd? (- x 1)))
  )

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; INPUT: funtion that takes one argument as generic type (A) yields boolean and list of generic type (A), OUTPUT: Boolean
;; RETURNS: boolean true if every member in the list yeiled true in the function 
;; OPERATION: returns false as soon as any member on the function yeiled false or continues recursivly untill the end and returns true 
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst)) (every? pred (rest lst)))
      )
  )

;; An example for the usefulness of this polymorphic function
(: all-even? :   (Listof Natural) -> Boolean)
;; INPUT: list of natural, OUTPUT: boolean
;; RETURNS: true if every member in the list is even, otherwise, flase
;; OPERATION: implementing every? using is-even? functions 
(define (all-even? lst)
  (every? is-even? lst)
  )

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6)))) 
 
 (: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;; INPUT: two functions each yeilding boolean and takes an arguments of (A) and (B) respectivly
;;   , and list of (A) and (B) resoectivly, where (A) and (B) are generic types , OUTPUT: Boolean
;; RETURNS: boolean true if every member in the list yeiled true in the function 
;; OPERATION: returns false as soon as any member on the function yeiled false or continues recursivly untill the end and returns true 
(define (every2? pred1 pred2  lst1 lst2)
  (or (null? lst1)
      ;; both lists assumed to be of same length
      (and (pred1 (first lst1)) (pred2 (first lst2)) (every2? pred1 pred2 (rest lst1) (rest lst2)))
      )
  ) 
 
 
 
 