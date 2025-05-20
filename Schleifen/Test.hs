module Main where

import Loops
import HaskellUnit

main = runTests [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19]

(!-!) (x:_) 0 = x
(!-!) (_:xs) n = xs !-! (n-1)
(!-!) _ _  = 0

prim2 = sieb2$from 2
test1 = testcase "(sieb2$from 2)!!0 " "(sieb2$from 2)!!0 falsch" 2 (prim2!-!0)
test2 = testcase "(sieb2$from 2)!!1 " "(sieb2$from 2)!!1 falsch" 3 (prim2!-!1)
test3 = testcase "(sieb2$from 2)!!5 " "(sieb2$from 2)!!5 falsch" 13 (prim2!-!5)

test4 = testcase "quersumme 11" "(quersumme 11) falsch" 2 (quersumme 11)
test5 = testcase "quersumme 1" "(quersumme 1) falsch" 1 (quersumme 1)
test6 = testcase "quersumme 1234" "(quersumme 1234) falsch" 10 (quersumme 1234)

test7 = testcase "ma1 (+1) [1,2,3]" "(ma1 (+1) [1,2,3]) falsch" [2,3,4] (ma1 (+1) [1,2,3])
test8 = testcase "ma1 (\\x->x*x) [1,2,3]" "(ma1 (\\x->x*x) [1,2,3]) falsch" [1,4,9] (ma1 (\x->x*x) [1,2,3])
test9 = testcase "ma1 (\\x->x*x) []" "(ma1 (\\x->x*x) []) falsch" ([]::[Integer]) (ma1  (\x->x*x) [])

test10 = testcase "contains 2 [1,2,3]" "(contains 2 [1,2,3]) falsch" True (contains 2 [1,2,3])
test11 = testcase "contains 3 [1,2,3]" "(contains 3 [1,2,3]) falsch" True (contains 3 [1,2,3])
test12 = testcase "contains 4 [1,2,3]" "(contains 4 [1,2,3]) falsch" False (contains 4 [1,2,3])

test13 = testcase "readNumber 1234" "(readNumber \"1234\") falsch" 1234 (readNumber "1234")
test14 = testcase "readNumber 101" "(readNumber \"101\") falsch" 101 (readNumber "101")
test15 = testcase "readNumber 00" "(readNumber \"00\") falsch" 0 (readNumber "00")

test16 = testcase "toBinary 2" "(toBinary 2) falsch" "10" (toBinary 2)
test17 = testcase "toBinary 3" "(toBinary 3) falsch" "11" (toBinary 3)
test18 = testcase "toBinary 42" "(toBinary 42) falsch" "101010" (toBinary 42)

test19 = testcase "woerter 1" "woerter \"eins    zwei     drei    \"" ["eins","zwei","drei"] (woerter "eins    zwei     drei    ")
