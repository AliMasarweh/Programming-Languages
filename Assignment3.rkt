#lang pl 03

#|
BNF for the ROL language:
       <ROL>  ::=  { reg-len = <Num> <RegE> }

       <RegE> ::=  <Bits>
                  | {and <RegE> <RegE>}
                  | {or <RegE> <RegE>}
                  | {shl <RegE>}
                  | {with {<id> <RegE>} <RegE>}
                  | {if <Bool> <RegE> <RegE>}
                  | <id>

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
	[With Symbol RegE RegE]
	[Id Symbol]
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

#| 
(: And-Bits : Bit-List Bit-List -> Bit-List)
(define (And-Bits bit-LstL bit-LstR)
  (: And-Bits-Tail : Bit-List Bit-List Bit-List -> Bit-List)
  (define (And-Bits-Tail bit-LstL bit-LstR ans)
    (cond
      [(null? bit-LstL) ans]
      [else
       (And-Bits-Tail (rest bit-LstL) (rest bit-LstR)
                      (append ans
                              (list (if (and (equal? (first bit-LstL) 1) (equal? (first bit-LstR) 1)) 1 0)))
                      )]
                      )
    )
  (And-Bits-Tail bit-LstL bit-LstR null)
  )

;;
(: Or-Bits : Bit-List Bit-List -> Bit-List)
(define (Or-Bits bit-LstL bit-LstR)
  (: Or-Bits-Tail : Bit-List Bit-List Bit-List -> Bit-List)
  (define (Or-Bits-Tail bit-LstL bit-LstR ans)
    (cond
      [(null? bit-LstL) ans]
      [else
       (Or-Bits-Tail (rest bit-LstL) (rest bit-LstR)
                      (append ans
                              (list (if (or (equal? (first bit-LstL) 1) (equal? (first bit-LstR) 1)) 1 0)))
                      )]
                      )
    )
  (Or-Bits-Tail bit-LstL bit-LstR null)
  )

;;
(: Shift-Lift-Bits : Bit-List -> Bit-List)
(define (Shift-Lift-Bits bit-List)
  (append (rest bit-List) (list (first bit-List)))
  )
|#


(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
;; Also validates the main format of the Sexpression
(define (parse-sexpr sexpr)
	(match sexpr
		[(list 'reg-len ‘=	 (number: n)	 args)
 			(if (> n 0)
       				(parse-sexpr-RegL args n)
    				(error 'parse-sexpr "Register length must be at least 1 ~s" sexpr) )]
		[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))





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
        [(symbol: id-name) (Id id-name)]  
        [(cons 'with args)
 	(match sexpr
     		[(list 'with (list (symbol: oldName) newName) body)
     			 (With oldName (parse-sexpr-RegL newName len) (parse-sexpr-RegL body len))]
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
            [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
               


;;parse-sexpr-RegL tests
;;; Tests ;;;
(test (parse-sexpr-RegL  (string->sexpr "{1 0 0 0}") 4) => (Reg '(1 0 0 0)))
(test (parse-sexpr-RegL  (string->sexpr  "{shl {1 0 0 0}}") 4) => (Shl (Reg '(1 0 0 0))))

(test (parse-sexpr-RegL (string->sexpr  
                  "{ or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}") 4)
      => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))

(test (parse-sexpr-RegL (string->sexpr "{with {x {1 1 1 1}} {shl y}}") 4)
      => (With 'x (Reg '(1 1 1 1)) (Shl (Id 'y))))
(test (parse-sexpr-RegL (string->sexpr " 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}") 2)
      => (With 'x (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))) (Shl (Id 'x))))
(test (parse-sexpr-RegL (string->sexpr 
                  "{if false {shl {1 0 1 1}} {1 1 0 1}}") 4)
      => (If (Bool false) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

(test (parse-sexpr-RegL (string->sexpr
                  "{if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}") 4)
      => (If (Maj (Reg '(0 0 1 1))) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

;;; Tests ;;;


(: parse : String -> RegE)
  ;; parses a string containing an AE expression to an AE AST
  ;; acts as a warpper for 'parse-sexpr-RegL & 'parse-sexpr
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

;;Parsing tests
;;; Tests ;;;
(test (parse  "{ reg-len =  4  {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse  "{ reg-len = 4  {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))

(test (parse "{ reg-len = 4  
                  { or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}}")
      => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))

(test (parse "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")
      => (With 'x (Reg '(1 1 1 1)) (Shl (Id 'y))))
(test (parse "{ reg-len = 2 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}}")
      => (With 'x (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))) (Shl (Id 'x))))

(test (parse "{ reg-len = 0 {}}") =error> 
            "Register length must be at least 1")

(test (parse "{ reg-len = 4 
                  {if false {shl {1 0 1 1}} {1 1 0 1}}}")
      => (If (Bool false) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

(test (parse "{ reg-len = 4 
                  {if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}}")
      => (If (Maj (Reg '(0 0 1 1))) (Shl (Reg '(1 0 1 1))) (Reg '(1 1 0 1))))

(test (parse "{ reg-len =  4 {maj? {0 0 1 1}} }") =error> "parse-sexpr: bad syntax in (maj? (0 0 1 1)")

;;; Tests ;;;

;; Wrapper of the result
(define-type RES
  [RegV Bit-List]
  [BoolV Boolean])



(: subst : RegE Symbol RegE -> RegE)
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define (subst expr from to) ; returns expr[to/from]
    (cases expr
      [(Reg n) expr]
      [(Bool b) expr]
      [(And regExprL regExprR) (And (subst regExprL from to) (subst regExprR from to))]
      [(Or regExprL regExprR) (Or (subst regExprL from to) (subst regExprR from to))]
      [(Shl regExpr) (Shl (subst regExpr from to))]
      [(With bound-id named-expr bound-body)
       (With bound-id
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
      [(Id name) (if (eq? name from) to expr)]
      [(If bool regExprDo regExprEl)
       (If (subst bool from to) (subst regExprDo from to) (subst regExprEl from to))]
      [(Geq regExprL regExprR) (Geq (subst regExprL from to) (subst regExprR from to))]
      [(Maj regExprL) (Maj (subst regExprL from to))]))



;;
(: eval : RegE -> RES)
(define (eval regExp)
  ;;extracts the wrapped boolean value in RES
  ;;like RegV->bit-list 
  (: RegV->boolean : RES -> Boolean)
  (define (RegV->boolean boolExpr)
    (cases boolExpr
      [(BoolV boolExpr) boolExpr]
      [else true]))
  (cases regExp
      	[(Reg BitLst) (RegV BitLst)]
	[(And RegExprL RegExprR) (reg-arith-op bit-and (eval RegExprL) (eval RegExprR))]
	[(Or RegExprL RegExprR) (reg-arith-op bit-or (eval RegExprL) (eval RegExprR))]
	[(Shl RegExpr) (RegV (shift-left (RegV->bit-list (eval RegExpr))))]
        [(With bound-id named-expr bound-body)
         (eval (subst bound-body bound-id (Reg (RegV->bit-list (eval named-expr)))))]
	[(Id name) (error 'eval "free identifier: ~s" name)]
        [(Bool b) (BoolV b)]
        [(Geq RegExprL RegExprR)
         (BoolV (geq-bitlists? (RegV->bit-list (eval RegExprL)) (RegV->bit-list (eval RegExprR))))]
        [(Maj RegExpr) (BoolV (majority? (RegV->bit-list (eval RegExpr))))]
        [(If boolExpr regDo regElse)
         (if (RegV->boolean (eval boolExpr)) (eval regDo) (eval regElse))]))



;;
(: run : String -> Bit-List)
  ;; evaluate an AE program contained in a string
  (define (run str)
    (RegV->bit-list (eval (parse str))))



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
      [(BoolV boolean) (error 'Reg-V "expected a register/bits, got boolean ~s" boolean)])) 


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
(test (reg-arith-op bit-or (RegV '(1 1 1 1)) (RegV '(0 1 0 0)))
      => (RegV '(1 1 1 1)))
(test (reg-arith-op bit-and (RegV '(1 1 1 1)) (RegV '(0 1 1 0)))
      => (RegV '(0 1 1 0)))

(test (reg-arith-op bit-or (RegV '(1 0 1 0))
 (reg-arith-op bit-and (RegV '(1 1 1 1)) (RegV '(0 1 1 0)))
 )
      => (RegV '(1 1 1 0)))

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

(test (parse "{ reg-len =  4  {1 0 0 0}}") => (Reg (list 1 0 0 0)))


  (test (run "{ reg-len =  4  {1 0 0 0}}") => '(1 0 0 0))  
  (test (run "{ reg-len = 4  {shl {1 0 0 0}}}") => '(0 0 0 1)) 
  (test (run "{ reg-len = 4   
                  {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") => '(0 1 0 1)) 
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0)) 
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1)) 
(test (run "{ reg-len = 4  
                 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>  
   '(0 1 0 1)) 
(test (run "{ reg-len = 4  
                  { or {and {shl {1 0 1 0}}  {shl {1 0 0 1}}}  
                       {1 0 1 0}}}") =>    '(1 0 1 1)) 
(test (run "{ reg-len = 2  
                  { or {and {shl {1 0}} {1 0}} {1 0}}}") =>  
         '(1 0))

(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")    
        =error> "free identifier: y")

(test (run "{ reg-len = 2 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}}") => '(0 1))
 
(test (run "{ reg-len = 4  {or {1 1 1 1} {0 1 1}}}") =error> 
            "wrong number of bits in (0 1 1)") 
(test (run "{ reg-len = 0 {}}") =error> 
            "Register length must be at least 1")


 
(test (run "{ reg-len = 3 
               {if {geq? {1 0 1} {1 1 1}}  
                   {0 0 1}  
                   {1 1 0}}}") => '(1 1 0)) 
(test (run "{ reg-len = 4 
                  {if {maj? {0 0 1 1}}  
                      {shl {1 0 1 1}}  
                      {1 1 0 1}}}")  => '(0 1 1 1)) 
(test (run "{ reg-len = 4 
                  {if false {shl {1 0 1 1}} {1 1 0 1}}}") => 
          '(1 1 0 1))  
;;







