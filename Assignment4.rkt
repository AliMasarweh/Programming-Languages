#lang pl

#|
;; *** Question 1 ***

BNF for the FROL language:
       <FROL>  ::=  { reg-len = <Num> <RegE> }

       <RegE> ::=  <Bits>
                  | {and <RegE> <RegE>}
                  | {or <RegE> <RegE>}
                  | {shl <RegE>}
                  | {with {<id> <RegE>} <RegE>}
                  | {if <Bool> <RegE> <RegE>}
                  | <id>
                  | { fun { <id> } <RegE> }
                  | { call <RegE> <RegE> }

       <Bool> ::=  {geq? <RegE> <RegE>}
                 | {maj? <RegE>}

       <Bits> ::= <Bit>
                 | <Bits> <Bit>
|#  
 

;;Defining the type BIT
(define-type BIT = (U 0 1))

;;Defining Bits
(define-type Bit-List = (Listof BIT))

;;COmmentS-Different-Approach
;;(define-type BoolReg = (U RegE Boolean))

;; Defining Register Experission
(define-type RegE
	[Reg Bit-List]
	[And RegE RegE]
	[Or RegE RegE]
	[Shl RegE]
	[WithFROL Symbol RegE RegE]
	[IdFROL Symbol]
        ;; Adding fun & call to the type
        [FunFROL  Symbol RegE]
        [CallFROL RegE RegE]
        [Bool Boolean]
        [Geq RegE RegE]
        [Maj RegE]
        [If RegE RegE RegE])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))])) 


(: parse-sexprFROL : Sexpr -> RegE)
;; to convert the main s-expression into FROL
;; Also validates the main format of the Sexpression
(define (parse-sexprFROL sexpr)
	(match sexpr
		[(list 'reg-len ‘=	 (number: n)	 args)
 			(if (> n 0)
       				(parse-sexpr-RegL args n)
    				(error 'parse-sexprFROL "Register length must be at least 1 ~s" sexpr) )]
		[else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))





(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; recusive parsing of Sexprission by the given number of bits
;; reduces the Sexprission to Abstract Syntax Tree
(define (parse-sexpr-RegL sexpr len)
	(match sexpr
	[(list (and a (or 1 0)) ... ) 
		(if (= len (length a))
			 (Reg (list->bit-list a))
		 	(error 'parse-sexpr-RegE "wrong number of bits in ~s" a)) ]
	[(list 'and regExprL regExprR) (And (parse-sexpr-RegL regExprL len) (parse-sexpr-RegL regExprR len))]
	[(list 'or regExprL regExprR) 	 (Or (parse-sexpr-RegL regExprL len) (parse-sexpr-RegL regExprR len))]
	[(list 'shl regExpr)		 (Shl (parse-sexpr-RegL regExpr len))]
        [(symbol: id-name) (IdFROL id-name)]
        [(cons 'with args)
 	(match sexpr
     		[(list 'with (list (symbol: oldName) newName) body)
     			 (WithFROL oldName (parse-sexpr-RegL newName len) (parse-sexpr-RegL body len))]
          [else (error 'parse-sexpr-RegE "bad `with' syntax in ~s" sexpr)])]
        [(list 'maj regExprL) 	 (Maj (parse-sexpr-RegL regExprL len))]
        [(list 'geq regExprL regExprR) 	 (Geq (parse-sexpr-RegL regExprL len) (parse-sexpr-RegL regExprR len))]
        [(cons 'if args)
         (match sexpr
           [(list 'if cond regExprDo regExprElse)
            (If
             (match cond
               ['true (Bool true)]
               ['false (Bool false)]
               [(list 'geq? regExp1 regExp2) (Geq (parse-sexpr-RegL regExp1 len) (parse-sexpr-RegL regExp2 len))]
               [(list 'maj? regExp) (Maj (parse-sexpr-RegL regExp len))]
               [else (parse-sexpr-RegL cond len)])
               (parse-sexpr-RegL regExprDo len) (parse-sexpr-RegL regExprElse len))])]
          ;; parsing fun & call
          [(list 'call fun arg) (CallFROL (parse-sexpr-RegL fun len) (parse-sexpr-RegL arg len))]
          [(cons 'fun more)
           (match sexpr
             [(list 'fun (list (symbol: name)) body)
              (FunFROL name (parse-sexpr-RegL body len))]
             [else (error 'parse-sexpr-RegL "bad `fun' syntax in ~s" sexpr)])]

            [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))
               


;;parse-sexpr-RegL tests
;;; Tests ;;;
#| old tests
(test (parse-sexpr-RegL  (string->sexpr "{1 0 0 0}") 4) => (Reg '(1 0 0 0)))
(test (parse-sexpr-RegL  (string->sexpr  "{shl {1 0 0 0}}") 4) => (Shl (Reg '(1 0 0 0))))

(test (parse-sexpr-RegL (string->sexpr  
                  "{ or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}") 4)
      => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))

(test (parse-sexpr-RegL (string->sexpr "{with {x {1 1 1 1}} {shl y}}") 4)
      => (WithFROL 'x (Reg '(1 1 1 1)) (Shl (IdFROL 'y))))
(test (parse-sexpr-RegL (string->sexpr " 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}") 2)
      => (WithFROL 'x (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))) (Shl (IdFROL 'x))))
(test (parse-sexpr-RegL (string->sexpr 
                  "{if false {shl {1 0 1 1}} {1 1 0 1}}") 4)
      => (If (Bool false) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

(test (parse-sexpr-RegL (string->sexpr
                  "{if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}") 4)
      => (If (Maj (Reg '(0 0 1 1))) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))
|#

;;; Tests ;;;


(: parseFROL : String -> RegE)
  ;; parses a string containing an AE expression to an AE AST
  ;; acts as a warpper for 'parse-sexpr-RegL & 'parse-sexpr
  (define (parseFROL str)
    (parse-sexprFROL (string->sexpr str)))

;;Parsing tests
;;; Tests ;;;
#|
Older tests
(test (parseFROL  "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parseFROL  "{ reg-len = 4  {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))

(test (parseFROL "{ reg-len = 4  
                  { or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}}")
      => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))

(test (parseFROL "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")
      => (WithFROL 'x (Reg '(1 1 1 1)) (Shl (IdFROL 'y))))
(test (parseFROL "{ reg-len = 2 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}}")
      => (WithFROL 'x (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))) (Shl (IdFROL 'x))))

(test (parseFROL "{ reg-len = 0 {}}") =error> 
            "Register length must be at least 1")

(test (parseFROL "{ reg-len = 4 
                  {if false {shl {1 0 1 1}} {1 1 0 1}}}")
      => (If (Bool false) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

(test (parseFROL "{ reg-len = 4 
                  {if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}}")
      => (If (Maj (Reg '(0 0 1 1))) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))


(test (parseFROL "{ reg-len =  4 {maj? {0 0 1 1}} }") =error> "parse-sexprFROL: bad syntax in (maj? (0 0 1 1))")
|#

;;new tests
 
(test (parseFROL "{ reg-len =  4 {call {fun {x} x} {0 1 0 1}}}") => (CallFROL (FunFROL 'x (IdFROL 'x)) (Reg '(0 1 0 1))))
(test (parseFROL "{ reg-len =  4 {call {fun {x} {shl x}} {0 1 1 1}}}") => (CallFROL (FunFROL 'x (Shl (IdFROL 'x))) (Reg '(0 1 1 1))))
(test (parseFROL "{ reg-len =  4 {call {fun {y} y} {call {fun {x} x} {0 1 0 1}}}}")
      => (CallFROL (FunFROL 'y (IdFROL 'y)) (CallFROL (FunFROL 'x (IdFROL 'x)) (Reg '(0 1 0 1)))))
(test (parseFROL "{ reg-len =  4 {with {foo {fun {x} {shl x}}} {call foo {1 1 0 1}}}}")
      => (WithFROL 'foo (FunFROL 'x (Shl (IdFROL 'x))) (CallFROL (IdFROL 'foo) (Reg '(1 1 0 1)))))

;;; Tests ;;;

;; Wrapper of the result
(define-type RES
  [RegV Bit-List]
  [FunV Symbol RegE]
  [BoolV Boolean])



(: substFROL : RegE Symbol RegE -> RegE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (substFROL expr from to) ; returns expr[to/from]
    (cases expr
      [(Reg n) expr]
      [(Bool b) expr]
      [(And regExprL regExprR) (And (substFROL regExprL from to) (substFROL regExprR from to))]
      [(Or regExprL regExprR) (Or (substFROL regExprL from to) (substFROL regExprR from to))]
      [(Shl regExpr) (Shl (substFROL regExpr from to))]
      [(WithFROL bound-id named-expr bound-body)
       (WithFROL bound-id
             (substFROL named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (substFROL bound-body from to)))]
      [(IdFROL name) (if (eq? name from) to expr)]
      [(If bool regExprDo regExprEl)
       (If (substFROL bool from to) (substFROL regExprDo from to) (substFROL regExprEl from to))]
      [(Geq regExprL regExprR) (Geq (substFROL regExprL from to) (substFROL regExprR from to))]
      [(Maj regExprL) (Maj (substFROL regExprL from to))]
      ;; substituting every free identifier in experssion 
      [(CallFROL l r) (CallFROL (substFROL l from to) (substFROL r from to))]
      [(FunFROL bound-id bound-body)
       (if (eq? bound-id from)
         expr
         (FunFROL bound-id (substFROL bound-body from to)))]
))

(: Res->RegE : RES -> RegE)
  (define (Res->RegE funExprWrapped)
    (cases funExprWrapped
      [(FunV symb fun) (FunFROL symb fun)]
      [(RegV lst) (Reg lst)]
      [(BoolV b) (Bool b)]))

;;
(: evalFROL : RegE -> RES)
(define (evalFROL regExp)
  ;;extracts the wrapped boolean value in RES
  ;;like RegV->bit-list 
  (: RegV->boolean : RES -> Boolean)
  (define (RegV->boolean boolExpr)
    (cases boolExpr
      [(BoolV boolExpr) boolExpr]
      [else true]))
  (cases regExp
      	[(Reg BitLst) (RegV BitLst)]
	[(And RegExprL RegExprR) (reg-arith-op bit-and (evalFROL RegExprL) (evalFROL RegExprR))]
	[(Or RegExprL RegExprR) (reg-arith-op bit-or (evalFROL RegExprL) (evalFROL RegExprR))]
	[(Shl RegExpr) (RegV (shift-left (RegV->bit-list (evalFROL RegExpr))))]
        [(WithFROL bound-id named-expr bound-body)
         (evalFROL (substFROL bound-body bound-id  (Res->RegE (evalFROL named-expr))))]
	[(IdFROL name) (error 'evalFROL "free identifier: ~s" name)]
        [(Bool b) (BoolV b)]
        [(Geq RegExprL RegExprR)
         (BoolV (geq-bitlists? (RegV->bit-list (evalFROL RegExprL)) (RegV->bit-list (evalFROL RegExprR))))]
        [(Maj RegExpr) (BoolV (majority? (RegV->bit-list (evalFROL RegExpr))))]
        [(If boolExpr regDo regElse)
         (if (RegV->boolean (evalFROL boolExpr)) (evalFROL regDo) (evalFROL regElse))]
        ;; Evaluating fun & call
        [(FunFROL bound-id bound-body) (FunV bound-id bound-body)]
        [(CallFROL fun-expr arg-expr)
         (let ([fval (evalFROL fun-expr)])
           (cases fval
             [(FunV bound-id bound-body)
              (evalFROL (substFROL bound-body
                           bound-id
                           (Res->RegE (evalFROL arg-expr))))]
             [else (error 'evalFROL "`call' expects a function, got: ~s"
                                fval)]))]))



;;
(: runFROL : String -> Bit-List)
  ;; evaluate an AE program contained in a string
  (define (runFROL str)
    (RegV->bit-list (evalFROL (parseFROL str))))



;; Defining functions for dealing with arithmetic operations
;; on the above types
(: bit-and : BIT BIT -> BIT)
;; Arithmetic and
(define(bit-and a b)
  (if (and (equal? a 1) (equal? b 1)) 1 0))


(: bit-or : BIT BIT -> BIT)
;; Aithmetic or
(define(bit-or a b)
  (if (or (equal? a 1) (equal? b 1)) 1 0))


(: RegV->bit-list : RES -> Bit-List)
  ;; extract a bit-list from RES type
(define (RegV->bit-list register)
  (cases register
      [(RegV reg) reg]
      [(BoolV boolean) (error 'Reg-V "expected a register/bits, got boolean ~s" boolean)]
      [(FunV symbol fun) (error 'Reg-V "expected a register/bits, got function ~s" fun)])) 


(: reg-arith-op : (BIT BIT -> BIT)  RES  RES ->  RES)
;; Consumes two registers and some binary bit operation 'op',
;; and returns the register obtained by applying op on the
;; i'th bit of both registers for all i.
(define (reg-arith-op op reg1 reg2)
  (: bit-arith-op : Bit-List Bit-List -> Bit-List)
  ;; Consumes two bit-lists and uses the binary bit operation 'op'.
  ;; It returns the bit-list obtained by applying op on the
  ;; i'th bit of both registers for all i.
  (define (bit-arith-op bl1 bl2)
    ;;
    (: bit-arith-op-helper : Bit-List Bit-List Bit-List -> Bit-List)
    (define (bit-arith-op-helper bl1 bl2 bl3)
      (cond
        [(null? bl1) bl3]
        [else (bit-arith-op-helper (rest bl1) (rest bl2)
                      (append bl3 (list (op (first bl1) (first bl2)))))]))
    (bit-arith-op-helper bl1 bl2 null))
    (RegV (bit-arith-op  (RegV->bit-list reg1) (RegV->bit-list reg2))))

;;reg-arith-op test
;;; Tests ;;;
#| old tests
(test (reg-arith-op bit-or (RegV '(1 1 1 1)) (RegV '(0 1 0 0)))
      => (RegV '(1 1 1 1)))
(test (reg-arith-op bit-and (RegV '(1 1 1 1)) (RegV '(0 1 1 0)))
      => (RegV '(0 1 1 0)))

(test (reg-arith-op bit-or (RegV '(1 0 1 0))
 (reg-arith-op bit-and (RegV '(1 1 1 1)) (RegV '(0 1 1 0)))
 )
      => (RegV '(1 1 1 0)))
|#

;;; Tests ;;;
 
 
(: majority? : Bit-List -> Boolean)
  ;; Consumes a list of bits and checks whether the
  ;; number of 1's are at least as the number of 0's.
  (define(majority? bl)
    (: majority-helper : Bit-List Number Number -> Boolean)
    (define (majority-helper bl zeros ones)
      (cond
        [(null? bl) (not (> zeros ones))]
        [(equal? (first bl) 0) (majority-helper (rest bl) (+ 1 zeros) ones)]
        [else (majority-helper (rest bl) zeros (+ 1 ones))]))
    (majority-helper bl 0 0))

 

(: geq-bitlists? : Bit-List Bit-List -> Boolean)
;; Consumes two bit-lists and compares them. It returns true if the
;; first bit-list is larger or equal to the second.
(define (geq-bitlists? bl1 bl2)
  (cond
    [(null? bl1) true]
    [(> (first bl1) (first bl2)) true]
    [(< (first bl1) (first bl2)) false]
    [else (geq-bitlists? (rest bl1) (rest bl2))]))  
 
(: shift-left : Bit-List -> Bit-List)
  ;; Shifts left a list of bits (once)
  (define(shift-left bl)
    (append (rest bl) (list (first bl))))

  
 







;; tests
#|

  old tests
  (test (runFROL "{ reg-len =  4  {1 0 0 0}}") => '(1 0 0 0))  
  (test (runFROL "{ reg-len = 4  {shl {1 0 0 0}}}") => '(0 0 0 1)) 
  (test (runFROL "{ reg-len = 4   
                  {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1)) 
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0)) 
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1)) 
(test (runFROL "{ reg-len = 4  
                 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>  
   '(0 1 0 1)) 
(test (runFROL "{ reg-len = 4  
                  { or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}}") =>    '(1 0 1 1)) 
(test (runFROL "{ reg-len = 2  
                  { or {and {shl {1 0}} {1 0}} {1 0}}}") =>  
         '(1 0))

(test (runFROL "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")    
        =error> "free identifier: y")

(test (runFROL "{ reg-len = 2 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}}") => '(0 1))
 
(test (runFROL "{ reg-len = 4  {or {1 1 1 1} {0 1 1}}}") =error> 
            "wrong number of bits in (0 1 1)") 
(test (runFROL "{ reg-len = 0 {}}") =error> 
            "Register length must be at least 1")


 
(test (runFROL "{ reg-len = 3 
               {if {geq? {1 0 1} {1 1 1}}  
                   {0 0 1}  
                   {1 1 0}}}") => '(1 1 0)) 
(test (runFROL "{ reg-len = 4 
                  {if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}}")  => '(0 1 1 1)) 
(test (runFROL "{ reg-len = 4 
                  {if false {shl {1 0 1 1}} {1 1 0 1}}}") => 
          '(1 1 0 1))  
;;
|#

;; new Tests
(test (runFROL "{ reg-len = 3
                  {with {identity {fun {x} x}}
                    {with {foo {fun {x} {or x {1 1 0}}}}
                      {call {call identity foo} {0 1 0}}}}}")
                 => '(1 1 0))
(test (runFROL "{ reg-len = 3
                  {with {x {0 0 1}}
                    {with {f {fun {y} {and x y}}}
                      {with {x {0 0 0}}
                        {call f {1 1 1}}}}}}")
                   => '(0 0 1)) 
 
(test (runFROL "{ reg-len = 4
                  {with {foo {fun {z} {if {maj? z} z {shl z}}}}
                    {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}")
                   => '(0 1 1 1)) 
 



(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 
 
(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str))) 
 
 

(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))])) 
 
(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arith-op "expects a number, got: ~s" e)]))
  (Num (op (Num->number expr1) (Num->number expr2)))) 
 
(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))])) 
 
(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [else (error 'run
                   "evaluation returned a non-number: ~s" result)]))) 

;; tests
#|
old tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
                {with {foo {fun {x} {+ x 1}}}
                  {call {call identity foo} 123}}}")
      => 124)
(test (run "{call {call {fun {x} {call x 1}}
                          {fun {x} {fun {y} {+ x y}}}}
                    123}")
      => 124)

|#

;;  *** Question 2 ***
(: CFSingle : String Symbol -> Natural)
(define (CFSingle expr name)
  (countFreeSingle (parse expr) name))

(: countFreeSingle : FLANG Symbol -> Natural)
(define (countFreeSingle expr symb)
  (cases expr
    ;;As a stop state, each same Id as symb is counted
    [(Id symbol) (if (eq? symbol symb) 1 0)]
    [(Num n) 0]
    ;;For binary operations, add the left-hand & right-hand operand's free Ids
    [(Add l r) (+ (countFreeSingle l symb) (countFreeSingle r symb))]
    [(Sub l r) (+ (countFreeSingle l symb) (countFreeSingle r symb))]
    [(Mul l r) (+ (countFreeSingle l symb) (countFreeSingle r symb))]
    [(Div l r) (+ (countFreeSingle l symb) (countFreeSingle r symb))]
    ;;If bound-id is the same as symb then the symbs in body,if any, are not free, 
    ;;count only the named-expr, otherwise, add both the named-expr & body's free-identifiers
    [(With bound-id named-expr bound-body) (if (eq? bound-id symb) (countFreeSingle named-expr symb)
                                               (+ (countFreeSingle named-expr symb) (countFreeSingle bound-body symb)))]
    ;;If bound-id is the same as symb, then the symbs, if any, in symb are not free => return 0
    ;;otherwise count in bound-body
    [(Fun bound-id bound-body) (if (eq? bound-id symb) 0 (countFreeSingle bound-body symb))]
    ;;Same as binary opeartors, there is no Ids to compare to.
    [(Call fun-expr arg-expr)
     (+ (countFreeSingle fun-expr symb) (countFreeSingle arg-expr symb))]))

;; new tests

(test (CFSingle "{+ r r}" 'r) => 2)
(test (CFSingle "{fun {r} {+ r e}}" 'e) => 1)
(test (CFSingle "{fun {r} {+ r e}}" 'r) => 0)
(test (CFSingle "{call {fun {r} {+ r e}}
                   {with {e {+ e r}}
                     {fun {x} {+ e r}}}}"
                'r) => 2) 
 
(test (CFSingle "{call {fun {r} {+ r e}}
                   {with {e {+ e r}}
                     {fun {x} {+ e r}}}}"
                'e) => 2)

;; 2.a
#|
According to CFSingle we have free 'x identifier,
but running the code won't throw an error, rather
it will run smoothly and return a value because
the substitution model run, will substitutes every
appearnce of 'foo (binding name) by it's body
(Add (Id x) (Id y)), then substitutes every appearnce
of 'x which results in a unexpected behavior 
(Dynamic scoping)
|#

(test (CFSingle "{with {foo {fun {y} {+ x y}}}
        {with {x 4}
          {call foo 3}}}" 'x) => 1)

(test (run "{with {foo {fun {y} {+ x y}}}
         {with {x 4}
           {call foo 3}}}") => 7)

#|
CFSingle returns 1, which means that x is a free identifier,
while during the evaluation by a mistake allocated a value
because of the substitution that occurs on 'foo before substituting
and evaluating & substituting x

(CFSingle "{with {foo {fun {y} {+ x y}}}
             {with {x 4}
              {call foo 3}}}" 'x)

(run "{with {foo {fun {y} {+ x y}}}
        {with {x 4}
          {call foo 3}}}")
|#

(define-type SubstCache = (Listof (List Symbol FLANG))) 
 
(: empty-subst : SubstCache)
(define empty-subst null) 
 
(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend name val sc)
  (cons (list name val) sc)) 
 
(: lookup : Symbol SubstCache -> FLANG) 
(define (lookup name sc)
  (let ([cell (assq name sc)])
    (if cell
        (second cell)
        (error 'lookup "no binding for ~s" name))))
(: counterx : Natural)
(define counterx 0)    ;;;above eval  
 
(: evalSC : FLANG SubstCache -> FLANG)
;; evaluates FLANG expressions by reducing them to expressions
(define (evalSC expr sc)
  (set! counterx (add1 counterx))
  (if (> counterx 500)
      (error 'eval "exceeded 500 times")
      (cases expr
        [(Num n) expr]
        [(Add l r) (arith-op + (evalSC l sc) (evalSC r sc))]
        [(Sub l r) (arith-op - (evalSC l sc) (evalSC r sc))]
        [(Mul l r) (arith-op * (evalSC l sc) (evalSC r sc))]
        [(Div l r) (arith-op / (evalSC l sc) (evalSC r sc))]
        [(With bound-id named-expr bound-body)
         (evalSC bound-body
                 (extend bound-id (evalSC named-expr sc) sc))]
        [(Id name) (lookup name sc)]
        [(Fun bound-id bound-body) expr]
        [(Call fun-expr arg-expr)
         (let ([fval (evalSC fun-expr sc)])
           (cases fval
             [(Fun bound-id bound-body)
              (evalSC bound-body
                      (extend bound-id (evalSC arg-expr sc) sc))]
             [else (error 'evalSC "`call' expects a function, got: ~s"
                          fval)]))]))) 
 
(: runSC : String -> Number)
;; evaluate a FLANG program contained in a string
(define (runSC str)
  (let ([result (evalSC (parse str) empty-subst)])
    (cases result
      [(Num n) n]
      [else (error 'runSC
                   "evaluation returned a non-number: ~s" result)]))) 


(define loop "{with {Ali {fun {x} {+ 1 {call Ali x}}}} {call Ali f}}")
(define loop2 "{with {x 100} f}")
(define loop3 "{with {loop {fun {x} {+ 1 {call loop x}}}} {call loop 1}}")
(define loop4 "{with {Ali {fun {x} {call Ali x}}} {call Ali 4}}")


(test (parse "{with {Ali {fun {x} {+ 1 {call Ali x}}}} {call Ali f}}")
      => (With 'Ali (Fun 'x (Add (Num 1) (Call (Id 'Ali) (Id 'x)))) (Call (Id 'Ali) (Id 'f))))

(test (let ([parsed (parse "{with {Ali {fun {x} {+ 1 {call Ali x}}}} {call Ali f}}")])
  (cases parsed
    [(With bound-id named-expr bound-body)
      (subst bound-body
                  bound-id
                  (eval named-expr))]
    [else (error 'parsed "lol")])) =>
       (Call (Fun 'x (Add (Num 1) (Call (Id 'Ali) (Id 'x)))) (Id 'f)))

;;First evaluates {'x 100} to substitues it's evaluation into 'f
;;Then evaluates 'f =error> free identifier: f
(test (run loop2) =error> "free identifier: f") ;; substitution model

;;In Cache Substitution model, the evaluation of each identifier is delayed 
;;untill the moment it's needed, then it's value is looked-up, which leads
;;only to extending the cache without looking-up for the free identifier
;;and reaching the limit
(test (runSC loop3) =error> "exceeded 500 times") ;; subst-cache model

;;First evaluates {'Ali {fun {'x} {+ 1 {call 'Ali 'x}}}} to substitues it's value into 'Ali in {call 'Ali 'f}
;;Then evaluates {call 'Ali 'f} which evaluates frist 'f to substitues it's value into 'Ali's argument
;;Which throws an error
(test (run loop) =error> "free identifier: f") ;; substitution model

;;In Cache Substitution model, the evaluation of each identifier is delayed 
;;untill the moment it's needed, then it's value is looked-up, which leads
;;only to extending the cache without looking-up for the free identifier
;;and reaching the limit
(test (runSC loop) =error> "exceeded 500 times") ;; subst-cache model

;;In any case, substituetion breaks the recursion once the function is substitued,
;; it loses it's body for the next substitution
(test (run loop4) =error> "eval: free identifier: Ali")


