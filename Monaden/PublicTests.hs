module PublicTests where

import Zustand
import HaskellUnit
import Data.Ratio
import Control.Monad.Trans.Maybe

tests = [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11]

iotestcase name msg expected actualIO  = do
  actual <- actualIO
  testcase name msg expected actual

rt n ex prog env = iotestcase n "" (Just ex) (fmap snd $ (runAnweisungT.runMaybeT.execute) (prog) env)
nt n prog env = iotestcase n "" (Nothing) (fmap snd $ (runAnweisungT.runMaybeT.execute) (prog) env)

t1 = rt "testli" 42 (Literal 42) []
t2 = rt "testvar" 42 (Var  "x") [("x",42)]
t3 = rt "testfun" 120 (fac) [("x",5)]
t4 = rt "testarith" 42 (Arith (Var  "x")(+)(Var  "x")) [("x",21)]
t5 = rt "testseq" 42 (Sequenz [Literal 42]) []
t6 = rt "test varseq" 42 (Sequenz [Var  "x"]) [("x",42)]
t7 = rt "test varseq 2" 42 (Sequenz [Var  "x",Literal 42]) [("x",42)]
t8 = nt "test var 3" (Var  "x") [("y",42)]
t9 = nt "test var 4" (Sequenz [Var  "x",Literal 42]) [("y",42)]
t10 = rt "test assign" 42 (Sequenz [Zuweisung "x" (Literal 42), Var  "x"]) [("x",42)]
t11 = rt "test assign 2" 42 (Sequenz [Zuweisung "x" (Arith (Var "x") (*)(Literal 2)), Var  "x"]) [("x",21)]

fac = Sequenz
 [Zuweisung "r" (Literal 1)
 ,While
    (Var "x")
    (Sequenz
      [Zuweisung "r" (Arith (Var "r")(*)(Var "x"))
      ,Zuweisung "x" (Arith (Var "x")(-)(Literal 1))
      ]
    )
 ,Var "r" 
 ]




