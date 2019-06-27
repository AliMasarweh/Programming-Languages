#lang pl

#|

Question 1


Context-free Grammar for String Expressions Language (Code)

**************************************************************************

The final output of the code is either a string, number or character,
so <SE> non-terminal can be replaced with number experssion (returns/represents a number),
a character, or a string expression (returns/represents a string).

StrExpr = string expression; CharSeq = characters sequence;
StrSeq = strings sequence; NumExpr = number experssion;
Str = string; Char = character;

**************************************************************************

<SE> ::= <NumExpr>                                       1
      | <Char>                                           2
      | <StrExpr>                                        3

<StrExpr> ::= <Str>                                      4
        | ( string <CharSeq>)                            5
        | ( string-append <StrSeq> )                     6
        | ( string-insert <StrExpr> <Char> <NumExpr> )   7
        | ( number->string <NumExpr> )                   8
  
<CharSeq> ::= <Char>                                     9
        | <CharSeq> <Char>                               10

<StrSeq> ::= <Str>                                       11
       | <StrSeq> <StrExpr>                              12

<Char> ::= #\<Digit>                                     13

<Str> ::= 𝜆  //(empty string)                            14
    | "<NumExpr>"                                        15

<NumExpr> ::= <Number>                                   16 
       | ( string-length <StrExpr> )                     17

<Number> ::= <Digit>                                     18
       | <Number><Digit>                                 19

<Digit> ::= 1                                            20
      | 2                                                21
      | 3                                                22
     .                                                  .
     .                                                  .
     .                                                  .
      | 8                                                27
      | 9                                                28


**************************************************************************

Examples:

1. ( string-insert ( number->string 15 )  #\4 66 )

<SE> (3) => <StrExpr> (7) => ( string-insert <StrExpr> <Char> <NumExpr> ) (17) => ( string-insert ( number->string <NumExpr> ) <Char> <NumExpr> )
(16) => ( string-insert ( number->string <Number> ) <Char> <NumExpr> )  (19) => ( string-insert ( number->string <Number> <Digit> ) <Char> <NumExpr> )
(18) => ( string-insert ( number->string <Digit><Digit> ) <Char> <NumExpr> ) (20,24) => ( string-insert ( number->string 15 ) <Char> <NumExpr> )
(13) => ( string-insert ( number->string 15 ) #\<Digit>  <NumExpr> )  (23) => ( string-insert ( number->string 15 ) #\4  <NumExpr> )
(16) => ( string-insert ( number->string 15 ) #\4  <Number> )  (19) => ( string-insert ( number->string 15 ) #\4  <Number> <Digit> )
(18) => ( string-insert ( number->string 15 ) #\4  <Digit><Digit> ) (25,25) =>  ( string-insert ( number->string 15 ) #\4  66 )


2. ( string-append ( string #\1 #\2 ) "33" )
<SE> (3) => <StrExpr> (5) => ( string-append <StrSeq> ) (12) => ( string-append <StrSeq> <StrExpr> ) (11) => ( string-append <StrExpr> <StrExpr> )
(4,5) => ( string-append ( string <CharSeq> ) <Str> ) (10,15) => ( string-append ( string <CharSeq> <Char> ) "<NumExpr>" )
(9,16) => ( string-append ( string <Char> <Char> ) "<Number>" )(13x2,19) => ( string-append ( string #\<Digit> #\<Digit> ) " <Number><Digit>" )
(21,18) => ( string-append ( string #\1 #\<Digit> ) " <Digit><Digit>" ) (22,23x2) => ( string-append ( string #\1 #\2 ) "33" )


**************************************************************************

Question 2

Given a MAE as :

<MAE> ::= <num> 
        | { + <MAE> <MAE> } 
        | { - <MAE> <MAE> } 
        | { * <MAE> <MAE> } 
        | { / <MAE> <MAE> } 
        | { set <MAE> } 
        | get 

One of the problem that this approach encouters is the ambiguity of evaluating these complicated expressions
(e.g. {* {+ {set 1} {set 2}} get} is evaluable to different values like 2 or 8,
 depending on which "set"'s value is evaluated and memorized last)
Avoiding the problem of ambiguity through enforcing the grammar to have "binding of get to set", or a set&get relation:

**************************************************************************

Approach 1:

<MAE-Set> ::= <MAE> 
        | { + <MAE-Set> <MAE> } 
        | { - <MAE-Set> <MAE> } 
        | { * <MAE-Set> <MAE> } 
        | { / <MAE-Set> <MAE> }
        | { set <MAE> }   //Once this set is derived, no more set will be available

<MAE> ::= <num> 
        | { + <MAE> <MAE> } 
        | { - <MAE> <MAE> } 
        | { * <MAE> <MAE> } 
        | { / <MAE> <MAE> } 
        | get


in this case, there is only one set, which limits the grammer a lot,
but sufficiently solves the above problem

**************************************************************************

Approach 2: Solving with sequence of sets.
The Objective of this approach is :preventing the first MAE from having get,
and the last MAE from having set.

<MAE>  ::= {seq <MAE-No-Get> }
         | {seq <AE> }

<MAE-No-Get> ::= { set <AE> } <MAE-Set-Get>

<MAE-Set-Get> ::= { set <MAE-Get> } <MAE-Set-Get>
        | <MAE-Get>

<MAE-Get> ::= <num> 
       | { + <MAE-Get> <MAE-Get> } 
       | { - <MAE-Get> <MAE-Get> } 
       | { * <MAE-Get> <MAE-Get> } 
       | { / <MAE-Get> <MAE-Get> }
       | get

<AE> ::= <num> 
       | { + <AE> <AE> } 
       | { - <AE> <AE> } 
       | { * <AE> <AE> } 
       | { / <AE> <AE> }


starting with <MAE-Seq>, two options are present, either Arithmetic Expression w/o memory.
Without memory is the same as <AE>, while with memory, the first part expression is obligated to not contain a "get",
then the in-between expression must start with "set" and can contain a "get", and the last one doesn't start with "set".

**************************************************************************

Examples:
123456789
207907171

1. {seq {set {+ 71 71}} 
     {set {* get get}} 
     {/ get 7}}

<MAE>  =>  {seq <MAE-No-Get> }  =>  {seq {set <AE> } <MAE-Set-Get> } => {seq {set {+ <AE> <AE> } } <MAE-Set-Get> }
=> {seq {set {+ <num> <num> } } <MAE-Set-Get> }  =>  {seq {set {+ 71 71 } } <MAE-Set-Get> }
=> {seq {set {+ 71 71 } } {set <MAE-Get> } <MAE-Set-Get> }  =>  {seq {set {+ 71 71 } } {set { * <MAE-Get> <MAE-Get> } } <MAE-Set-Get> }
=> {seq {set {+ 71 71}} {set {* get get}} <MAE-Set-Get>}  =>  {seq {set {+ 71 71}} {set {* get get}} <MAE-Get>}
=> {seq {set {+ 71 71}} {set {* get get}} {/ <MAE-Get> <MAE-Get>}}  =>  {seq {set {+ 71 71}} {set {* get get}} {/ get 7}}


2. {seq {- 7 0}}

<MAE>  =>  {seq <AE> }  =>  {seq {- <AE> <AE>} }  =>  {seq {- <num> <num>} }  =>  {seq {- 7 0} }

3.  {seq {set {+ 1 7}} 
     {set {* 1 4}} 
     {/ get 2}}
<MAE>  =>  {seq <MAE-No-Get> }  =>  {seq {set <AE> } <MAE-Set-Get> } => {seq {set {+ 1 7 } } <MAE-Set-Get> }
=> {seq {set {+ <num> <num> } } <MAE-Set-Get> }  =>  {seq {set {+ 1 71 } } <MAE-Set-Get> }
=> {seq {set {+ 1 7 } } {set <MAE-Get> } <MAE-Set-Get> }  =>  {seq {set {+ 1 7 } } {set { * <MAE-Get> <MAE-Get> } } <MAE-Set-Get> }
=> {seq {set {+ 1 7}} {set {* <num> <num>}} <MAE-Set-Get>}  => {seq {set {+ 1 7}} {set {* 1 4}} <MAE-Set-Get>} =>
{seq {set {+ 1 7}} {set {* 1 4}} <MAE-Get>} => {seq {set {+ 1 4}} {set {* 1 7}} {/ <MAE-Get> <MAE-Get>}}  =>  {seq {set {+ 1 7}} {set {* 1 4}} {/ get 2}}

|#


#|
Question 3
|#

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl (lambda ([a : Number] [b : Number]) (+ (* a a) b)) 0 lst)
  )

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 2 3 4)) => 30)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(2 3 4 5)) => 54)
(test (sum-of-squares '(3 4 5)) => 50)



#|
Question 4
|#

;; A function to help understanding createPolynomial
(: poly : (Listof Number) Number Integer Number -> Number)
(define (poly argsL x power accum)
  (: polyX : Number -> Number)
  (define (polyX x)
    x)
  (if (null? argsL)
      (polyX accum)
      (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))))
  )


(test (poly '(2 3 4 5)  0 0 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (poly '(2 3 4 5)  4 0 0) =>
      (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))
(test (poly '(2 3 4 5)  11 0 0) =>
      (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 

;; Returns a Polynomial with placeholder x, waiting to be fed values to return
;; the output of the function at that point
(: createPolynomial : (Listof Number) -> ( Number -> Number))
 (define (createPolynomial coeffs)
   (: poly : (Listof Number) Number Integer Number -> Number)
   (define (poly argsL x power accum)
     (if (null? argsL)
         accum
         (poly (rest argsL) x (+ power 1)
               (+ accum (* (first argsL) (expt x power)))
               )
         )
     )
     (: polyX : Number -> Number)
     (define (polyX x) (poly coeffs x 0 0))
     (lambda (y) (polyX y))
     )


(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  
   (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 
(test (p2345 4) =>  
   (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2)))) 
 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0)



#|


The grammar:
<PLANG> ::= { { poly <AEs>} { <AEs> } }

<AEs>   ::= <num>
          | <AEs> <num>

<AE> ::= <num> 
       | { + <AE> <AE> } 
       | { - <AE> <AE> } 
       | { * <AE> <AE> } 
       | { / <AE> <AE> }

|#


(define-type PLANG
  [Poly (Listof AE) (Listof AE)])
(define-type AE
  [Num  Number]
  [Add  AE AE]
  [Sub  AE AE]
  [Mul  AE AE]
  [Div  AE AE]
  )

(: parse-sexpr : Sexpr -> AE)   ;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


;; Helping Mapper
(: SexprList->AEList : (Listof Sexpr) -> (Listof AE))
(define (SexprList->AEList lst)
  (map (lambda ([num : Sexpr])
         (parse-sexpr num))
         lst)
  )

;; Match the code to the correct syntax then process the Sexpr into PLANG.
(: Match&Process-Code : Sexpr -> PLANG)
(define (Match&Process-Code code)
  (match code
    [(list lstL lstR)
     (cond
       [(or (equal? lstL '(poly)) (null? lstL)) (error 'parse-sexpr "parse: at least one coefficient is required in ~s" code)]
       [(null? lstR) (error 'parse-sexpr "parse: at least one point is required in ~s" code)]
       [(and (list? lstL) (list? lstR) (equal? (first lstL) 'poly))
        (Poly (SexprList->AEList (rest lstL)) (SexprList->AEList lstR))]
       [else (error 'parse-sexpr "bad syntax in ~s" code)]
       )
     ]
    )
  )

;; parses a string containing a PLANG expression to a PLANG AST
(: parse : String -> PLANG)  
(define (parse str)
 (let ([code (string->sexpr str)])
  (Match&Process-Code code)
   )
  )


(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }")  =error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")  =error> "parse: at least one point is required in ((poly 1 2) ())")


(: eval : AE -> Number)
;; consumes an AE and computes the corresponding number
(define (eval expr)
  (cases expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))


(: polynom-mapper : ( Number -> Number) (Listof Number) -> (Listof Number))
(define (polynom-mapper polynom-func vals)
  (map (lambda (val)
            (polynom-func val))
            vals)
  )

(: eval-poly : PLANG -> (Listof Number))
(define (eval-poly polynom)
  (cases polynom
    [(Poly coes vals)
     (polynom-mapper
      (createPolynomial
       (map (lambda (coe) (eval coe)) coes))
       (map (lambda (val) (eval val)) vals))]
    )
  )

(: run : String -> (Listof Number))
  ;; evaluate an AE program contained in a string
  (define (run str)
    (eval-poly (parse str)))


(test (run "{{poly 1 2 3} {1 2 3}}")  => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")   => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  => '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")  => '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))

#|     (let ([polynom (createPolynomial coes)])
       (map (lambda (val)
            (polynom val))
            vals))]
|#