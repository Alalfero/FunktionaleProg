module Main where
import Tree hiding (t1, t2)
import HaskellUnit

main = runTests [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21]

t1 = 56 +> 5 +> 67 +> 17 +> 4 +> -576 +> 42 +> 1000000078979887657 +> Empty
t3 = 54 +> 6 +> 66 +> 17 +> 4 +> -576 +> 42 +> 1000000078979887657 +> Empty
t4 = 17 +> 4 +> -576 +> 42 +> 1000000078979887657 +> Empty
t5 =  56 +> 5 +> 67 +>54 +> 6 +> 66 +> 17 +> 4 +> -576 +> 42 +> 1000000078979887657 +> Empty

t2  = "hallo" +> "welt" +> "witzebitzelbritz" +> "pandudel" +> "sowieso" +> "klaro" +> Empty
t6  = 5+>4+>16+>8+>7 +> Empty

test1 = testcase "size 1" "(size) falsch" 8 (size t1)
test2 = testcase "size 2" "(size Empty) falsch" 0 (size Empty)
test3 = testcase "size 3" "(size) falsch" 6 (size t2)

test4 = testcase "contains 1" "(contains) falsch" True (contains 67 t1)
test5 = testcase "contains 2" "(contains) falsch" False (contains 66 t1)
test6 = testcase "contains 3" "(contains) falsch" False (contains 67 Empty)
test7 = testcase "contains 4" "(contains) falsch" True (contains "pandudel" t2)

test8 = testcase "inorder 1" "(inorder) falsch" [-576,4,5,17,42,56,67,1000000078979887657] (inorder t1)
test9 = testcase "inorder 2" "(inorder) falsch" ["hallo","klaro","pandudel","sowieso","welt","witzebitzelbritz"] (inorder t2)
test10 = testcase "inorder 3" "(inorder) falsch" ([]::[Integer]) (inorder Empty)

test11 = testcase "remove 1" "(remove) falsch" t1 (remove 66 t1)
test12 = testcase "add 1" "(add) falsch. Element war schon im Baum." t1 (add 67 t1)
test13 = testcase "add 2" "(add) falsch." False (t1 == (add 66 t1))
test14 = testcase "remove 2" "(remove) falsch." False (t1 == (remove 67 t1))

test15 =  testcase "union 1" "Vereinigung mit sich selbst falsch." t1 (t1\/t1)
test16 =  testcase "intersection 1" "Schnitt mit sich selbst falsch." t1 (t1/\t1)

test17 =  testcase "union 2" "Vereinigung mit leerer Menge falsch." t1 (t1\/Empty)
test18 =  testcase "intersection 2" "Schnitt mit leerer Menge falsch." Empty (t1/\Empty)

test19 =  testcase "intersection 3" "Schnitt mit Menge falsch." t4 (t1/\t3)
test20 =  testcase "union 3" "Vereinigung mit Menge falsch." t5 (t1\/t3)

test21 =  testcase "tmap" "tmap falsch." t6 (tmap length t2)


