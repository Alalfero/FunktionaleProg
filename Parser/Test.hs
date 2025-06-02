module Main where

import OurParserLib
import HaskellUnit

main = runTests [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20]

p1 = "42a"
p2 = "17+4*2xx"
p3 = "17  +  4  *  2xx"
p4 = "(17+4)*2xx"
p5 = "(17+4)*  2 == 42 "
p6 = "(17+4)*  2 == 42 || 19<=0 && 1/=56"
p7 = "if (17+4)*  2 == 42 || 19<=0 && 1/=56 then 42 else 18"
p8 = "x"
p9 = "f(x,1,2,17+4)"
p10 = "f(x,1,f(2,g(x,f(y))),17+4)"
p11 = "fun fac(n)=if n<=0 then 1 else n*fac(n-1) fac(5)"
p12 = "fun fac(n)={r:=1;while(n>0){r:=r*n;n:=n-1;} r;} fac(5)"
p13 = "fun pre(n)=n-1fun fib(n)=if n<=1 then n else fib(pre(pre(n)))+fib(pre(n)) fib(10)"

pEt n ex res = testcase n  ("Parser Fehler für \""++ex++"\"") res (expr ex)

pPt n ex res = testcase n  ("Parser Fehler für \""++ex++"\"") res (prog ex)

rT n ex res =  testcase n  ("Ausführungsfehler \""++ex++"\"") res (runProg$fst$head$prog ex)

test1 = pEt "Expression1" p1  [(Number 42,"a")]
test2 = pEt "Expression2" p2  [(BinOp (Number 17) ADD (BinOp (Number 4) MULT (Number 2)),"xx")]
test3 = pEt "Expression3" p3  [(BinOp (Number 17) ADD (BinOp (Number 4) MULT (Number 2)),"xx")] 
test4 = pEt "Expression4" p4  [(BinOp (BinOp (Number 17) ADD (Number 4)) MULT (Number 2),"xx")]
test5 = pEt "Expression5" p5  [(BinOp (BinOp (BinOp (Number 17) ADD (Number 4)) MULT (Number 2)) OEQ (Number 42),"")]
test6 = pEt "Expression6" p6  [(BinOp (BinOp (BinOp (BinOp (BinOp (Number 17) ADD (Number 4)) MULT (Number 2)) OEQ (Number 42)) OR (BinOp (Number 19) LE (Number 0))) AND (BinOp (Number 1) NEQ (Number 56)),"")]
test7 = pEt "Expression7" p7  [(IfExpr (BinOp (BinOp (BinOp (BinOp (BinOp (Number 17) ADD (Number 4)) MULT (Number 2)) OEQ (Number 42)) OR (BinOp (Number 19) LE (Number 0))) AND (BinOp (Number 1) NEQ (Number 56))) (Number 42) (Number 18),"")]
test8 = pEt "Expression8" p8  [(Variable "x","")]
test9 = pEt "Expression9" p9  [(FunCall "f" [Variable "x",Number 1,Number 2,BinOp (Number 17) ADD (Number 4)],"")]
test10 = pEt "Expression10" p10 [(FunCall "f" [Variable "x",Number 1,FunCall "f" [Number 2,FunCall "g" [Variable "x",FunCall "f" [Variable "y"]]],BinOp (Number 17) ADD (Number 4)],"")]
test11 = pPt "Prog1" p11 [(Prog [Fun "fac" ["n"] [Simple (IfExpr (BinOp (Variable "n") LE (Number 0)) (Number 1) (BinOp (Variable "n") MULT (FunCall "fac" [BinOp (Variable "n") SUB (Number 1)])))]] (FunCall "fac" [Number 5]),"")]
test12 = pPt "Prog2" p12 [(Prog [Fun "fac" ["n"] [Assignment "r" (Number 1),While (BinOp (Variable "n") OGT (Number 0)) [Assignment "r" (BinOp (Variable "r") MULT (Variable "n")),Assignment "n" (BinOp (Variable "n") SUB (Number 1))],Simple (Variable "r")]] (FunCall "fac" [Number 5]),"")]
test13 = rT "Run1" p12 120
test14 = rT "Run2" p11 120
test15 = rT "Run3" p1 42
test16 = rT "Run4" p2 25
test17 = rT "Run5" p4 42
test18 = rT "Run6" p6 1
test19 = rT "Run7" p7 42
test20 = rT "Run8" p13 55

-- test = testcase "expr \"\"" "Parser Fehler" ()


