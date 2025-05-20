



> module Loops where
> import Data.Char





> for result test step calcResult v
>   |test v = for (calcResult result v) test step calcResult (step v)
>   |otherwise = result





> fac1 = for 1 (\x-> x>0) (\x->x-1) (*)





> sum1 = for 0  (not.null) tail      (\r xs -> r+head xs)





> len1 = for 0  (not.null) tail      (\r _ -> r+1)





> rev1 = for [] (not.null) tail      (\r xs -> head xs:r)





> for2 result test step calcResult v
>   |test v = (for2$!(calcResult result v)) test step calcResult (step v)
>   |otherwise = result





> fac3 = for2 1 (\x-> x>0) (\x->x-1) (*)






> fac2 n
>   |n<=0 = 1
>   |otherwise = n*fac (n-1)





> sum2 [] = 0
> sum2 (x:xs) = x+sum xs





> max2 [x] = x
> max2 (x:xs) = max x (max2 xs)





> fi2 _ [] = []
> fi2 p (x:xs)
>   |p x = x:fi2 p xs
>   |otherwise = fi2 p xs





> type TestFunction a = a -> Bool
> type BaseResult a r = a -> r
> type ResultFunction a r = a -> r -> r
> type StepFunction a  = a -> a





> loop ::
>      TestFunction a
>   -> BaseResult a r
>   -> ResultFunction a r
>   -> StepFunction a
>   -> a
>   -> r





> loop p baseResultF resultF step v
>  |p v = baseResultF v
>  |otherwise = resultF v (loop p baseResultF resultF step (step v))





> fac  = loop ((>=) 0) (const 1) (*) (\x->x-1)





> len  = loop null  (const 0) (\_ y -> y+1) tail 





> ma :: Ord a => [a] -> a
> ma   = loop (null.tail) head (\xs m -> max (head xs) m) tail





> fi p = loop null id  (\x y ->if p(head x) then (head x:y) else  y) tail





> la   = loop (null.tail) head (\_ y -> y) tail





> re   = loop null id (\x r -> r++[head x]) tail 





> from = loop (\x->False) (const []) (\x rs -> x:rs) ((+) 1)





> fromTo frm to = loop (\x->x>to) (const []) (\x rs -> x:rs) ((+) 1)frm





> quersumme n = for 0 (> 0) (`div` 10) (\r v -> r + v `mod` 10) n




> readNumber s = for 0 (not . null) tail (\r v -> r * 10 + digitToInt (head v)) s







> toBinary 0 = "0"
> toBinary n = for "" (> 0) (`div` 2) (\r v -> intToDigit (v `mod` 2) : r) n





> contains o xs= for False (not . null) tail (\found v -> found || head v == o) xs





> ma1 f  = loop null (const []) (\v rec -> f (head v) : rec) tail





> sieb2 = loop null (const []) (\v rec -> head v : rec) (\v -> filter (\y -> y `mod` head v /= 0) (tail v))





> woerter = snd.result " ".loop null (const ([],[])) result tail
>   where
>     result (x:xs) (w,ws) 
>       | isSpace x && not (null w) = ([],  w : ws)
>       | isSpace x                 = ([], ws)
>       | otherwise                 = (x : w, ws)

