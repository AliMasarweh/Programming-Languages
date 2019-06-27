#lang pl

#|
;; *** Question 1 ***

BNF for the FROL language:
       <ROL>  ::=  { reg-len = <Num> <RegE> }

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
(define-type ROL
	[Reg Bit-List]
	[And ROL ROL]
	[Or ROL ROL]
	[Shl ROL]
	[With Symbol ROL ROL]
	[Id Symbol]
        ;; Adding fun & call to the type
        [Fun Symbol ROL]
        [Call ROL ROL]
        [Bool Boolean]
        [Geq ROL ROL]
        [Maj ROL]
        [If ROL ROL ROL])

;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))])) 


(: parse-sexpr : Sexpr -> ROL)
;; to convert the main s-expression into ROL
;; Also validates the main format of the Sexpression
(define (parse-sexpr sexpr)
	(match sexpr
		[(list 'reg-len ‘=	 (number: n)	 args)
 			(if (> n 0)
       				(parse-sexpr-RegL args n)
    				(error 'parse-sexpr "Register length must be at least 1 ~s" sexpr) )]
		[else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))





(: parse-sexpr-RegL : Sexpr Number -> ROL)
;; recusive parsing of Sexprission by the given number of bits
;; reduces the Sexprission to Abstract Syntax Tree
(define (parse-sexpr-RegL sexpr len)
	(match sexpr
	[(list (and a (or 1 0)) ... ) 
		(if (= len (length a))
			 (Reg (list->bit-list a))
		 	(error 'parse-sexpr-RegE "wrong number of bits in ~s" a)) ]
	[(list 'and ROLExprL ROLExprR) (And (parse-sexpr-RegL ROLExprL len) (parse-sexpr-RegL ROLExprR len))]
	[(list 'or ROLExprL ROLExprR) 	 (Or (parse-sexpr-RegL ROLExprL len) (parse-sexpr-RegL ROLExprR len))]
	[(list 'shl ROLExpr)		 (Shl (parse-sexpr-RegL ROLExpr len))]
        [(symbol: id-name) (Id id-name)]
        [(cons 'with args)
 	(match sexpr
     		[(list 'with (list (symbol: oldName) newName) body)
     			 (With oldName (parse-sexpr-RegL newName len) (parse-sexpr-RegL body len))]
          [else (error 'parse-sexpr-RegE "bad `with' syntax in ~s" sexpr)])]
        [(list 'maj ROLExprL) 	 (Maj (parse-sexpr-RegL ROLExprL len))]
        [(list 'geq ROLExprL ROLExprR) 	 (Geq (parse-sexpr-RegL ROLExprL len) (parse-sexpr-RegL ROLExprR len))]
        [(cons 'if args)
         (match sexpr
           [(list 'if cond ROLExprDo ROLExprElse)
            (If
             (match cond
               ['true (Bool true)]
               ['false (Bool false)]
               [(list 'geq? ROLExp1 ROLExp2) (Geq (parse-sexpr-RegL ROLExp1 len) (parse-sexpr-RegL ROLExp2 len))]
               [(list 'maj? ROLExp) (Maj (parse-sexpr-RegL ROLExp len))]
               [else (parse-sexpr-RegL cond len)])
               (parse-sexpr-RegL ROLExprDo len) (parse-sexpr-RegL ROLExprElse len))])]
          ;; parsing fun & call
          [(list 'call fun arg) (Call (parse-sexpr-RegL fun len) (parse-sexpr-RegL arg len))]
          [(cons 'fun more)
           (match sexpr
             [(list 'fun (list (symbol: name)) body)
              (Fun name (parse-sexpr-RegL body len))]
             [else (error 'parse-sexpr-RegL "bad `fun' syntax in ~s" sexpr)])]

            [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
               

(: parse : String -> ROL)
  ;; parses a string containing an AE expression to an AE AST
  ;; acts as a warpper for 'parse-sexpr-RegL & 'parse-sexpr
  (define (parse str)
    (parse-sexpr (string->sexpr str)))


(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])


;; Wrapper of the result
(define-type VAL
  [RegV Bit-List]
  [BoolV Boolean]
  [FunV Symbol ROL ENV])


 (: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))])) 




(: VAL->ROL : VAL -> ROL)
  (define (VAL->ROL funExprWrapped)
    (cases funExprWrapped
      [(FunV symb fun env) (Fun symb fun)]
      [(RegV lst) (Reg lst)]
      [(BoolV b) (Bool b)]))

(: Syntactic-Sugar : ROL -> ROL)
(define (Syntactic-Sugar expr)
  (cases expr
    [(With bound-id named-expr bound-body) (Call (Fun bound-id bound-body) named-expr)]
    [else (error 'Syntactic-Sugar " asdasd ~s" expr)]))

;; test

(test (Syntactic-Sugar (With 'x (Reg '(1 0 1)) (Reg '(1 1 1))))
      => (Call (Fun 'x (Reg '(1 1 1))) (Reg '(1 0 1))))

(test (Syntactic-Sugar (With 'x (Reg '(1 0 1)) (Id 'x)))
      => (Call (Fun 'x (Id 'x)) (Reg '(1 0 1))))

(test (Syntactic-Sugar (With 'x
                       (Syntactic-Sugar (With 'y (Reg '(1 0 1)) (Shl (Id 'y))))
                       (Id 'x)))
      => (Call
          (Fun 'x (Id 'x))
          (Call (Fun 'y (Shl (Id 'y))) (Reg '(1 0 1)))))

(test (Syntactic-Sugar (With 'x
                       (Syntactic-Sugar (With 'y (Reg '(1 0 1)) (Shl (Id 'y))))
                       (Syntactic-Sugar (With 'z (Reg '(1 1 1)) (And (Id 'z) (Id 'x))))))
      =>(Call
         (Fun 'x (Call (Fun 'z (And (Id 'z) (Id 'x))) (Reg '(1 1 1))))
          (Call (Fun 'y (Shl (Id 'y))) (Reg '(1 0 1)))))





(: eval : ROL ENV -> VAL)
(define (eval ROLExp env)
  ;;extracts the wrapped boolean value in RES
  ;;like RegV->bit-list 
  (: RegV->boolean : VAL -> Boolean)
  (define (RegV->boolean boolExpr)
    (cases boolExpr
      [(BoolV boolExpr) boolExpr]
      [else true]))
  (cases ROLExp
      	[(Reg BitLst) (RegV BitLst)]
	[(And ROLExprL ROLExprR) (reg-arith-op bit-and (eval ROLExprL env) (eval ROLExprR env))]
	[(Or ROLExprL ROLExprR) (reg-arith-op bit-or (eval ROLExprL env) (eval ROLExprR env))]
	[(Shl ROLExpr) (RegV (shift-left (RegV->bit-list (eval ROLExpr env))))]
        [(With bound-id named-expr bound-body)
         (eval (Syntactic-Sugar ROLExp) env)]
	[(Id name) (lookup name env)] 
        [(Bool b) (BoolV b)]
        [(Geq ROLExprL ROLExprR)
         (BoolV (geq-bitlists? (RegV->bit-list (eval ROLExprL env)) (RegV->bit-list (eval ROLExprR env))))]
        [(Maj ROLExpr) (BoolV (majority? (RegV->bit-list (eval ROLExpr env))))]
        [(If boolExpr regDo regElse)
         (if (RegV->boolean (eval boolExpr env)) (eval regDo env) (eval regElse env))]
        ;; Evaluating fun & call
        [(Fun bound-id bound-body) (FunV bound-id bound-body env)]
        [(Call fun-expr arg-expr)
         (let ([fval (eval fun-expr env)])
           (cases fval
             [(FunV bound-id bound-body f-env)
              (eval bound-body
                    (Extend bound-id (eval arg-expr env) f-env))]
             [else (error 'eval "`call' expects a function, got: ~s"
                          fval)]))]))




;;
(: run : String -> Bit-List)
  ;; evaluate an AE program contained in a string
  (define (run str)
    (RegV->bit-list (eval (parse str) (EmptyEnv))))



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


(: RegV->bit-list : VAL -> Bit-List)
  ;; extract a bit-list from VAL type
(define (RegV->bit-list expr)
  (cases expr
      [(RegV reg) reg]
      [else (error 'Reg-V "expected a register/bits, got boolean ~s" expr)]))


(: reg-arith-op : (BIT BIT -> BIT)  VAL  VAL -> VAL)
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


;; new Tests
(test (run "{ reg-len = 3
                  {with {identity {fun {x} x}}
                    {with {foo {fun {x} {or x {1 1 0}}}}
                      {call {call identity foo} {0 1 0}}}}}")
                 => '(1 1 0))
(test (run "{ reg-len = 3
                  {with {x {0 0 1}}
                    {with {f {fun {y} {and x y}}}
                      {with {x {0 0 0}}
                        {call f {1 1 1}}}}}}")
                   => '(0 0 1)) 
 
(test (run "{ reg-len = 4
                  {with {foo {fun {z} {if {maj? z} z {shl z}}}}
                    {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}")
                   => '(0 1 1 1)) 

(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")    
        =error> "lookup: no binding for y")

(test (run "{ reg-len = 2 
                  { with {x { or {and {shl {1 0}}  
                                      {1 0}}  
                                 {1 0}}}   
                       {shl x}}}") => '(0 1))
