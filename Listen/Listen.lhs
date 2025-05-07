



> module Listen where





> import Data.List





> import Data.Ratio





> data Li a = E | a :> Li a deriving (Show,Eq)





> infixr 5 :>





> laengeLi E = 0
> laengeLi (_:>xs) = 1 + laengeLi xs





> laenge [] = 0
> laenge (_:xs) = 1 + laenge xs





> letzteLi (x:>E) = x
> letzteLi (_:>xs) = letzteLi xs





> letzte1 (x:[]) = x
> letzte1 (_:xs) = letzte1 xs





> letzte2 [x] = x
> letzte2 (_:xs) = letzte2 xs





> istSortiertLi (x1:>x2:>xs)
>   |x1<=x2 = istSortiertLi (x2:>xs)
>   |otherwise = False
> istSortiertLi _ = True





> istSortiert (x1:x2:xs)
>   |x1<=x2 = istSortiert (x2:xs)
>   |otherwise = False
> istSortiert _ = True





> istSortiert2 (x1:ys@(x2:xs))
>   |x1<=x2 = istSortiert2 ys
>   |otherwise = False
> istSortiert2 _ = True





> wiederholeLi x =  x:>wiederholeLi x 





> wiederhole x =  x:wiederhole x 





> factorial1 n = product [1,2..n]





> factorial2 n = product$take n [1,2..]





> isPalindrome :: Eq a => [a] -> Bool
> isPalindrome xs = xs == reverse xs 





> everyNth :: Integral a => a -> [b] -> [b]
> everyNth n xs = [x | (i, x) <- zip [0..] xs, i `mod` n == 0]





> swapNeighbours :: [a] -> [a]
> swapNeighbours [] = []
> swapNeighbours [x] = [x] 
> swapNeighbours (x:y:xs) = y : x : swapNeighbours xs





> maxRep :: Eq a => [a] -> Int
> maxRep [] = 0
> maxRep xs =  maximum (map length (group xs))





> leibniz:: [Rational]
> leibniz = [((-1)^k) % (2*k + 1) | k <- [0..]]





> fatio:: [Rational]
> fatio = [product [1..k] % product [1,3..(2*k + 1)] | k <- [0..]]





> squaresum :: (Foldable t, Num a, Functor t) => t a -> a
> squaresum  = foldr (\x acc -> x^2 + acc) 0 





> sqx :: (Fractional a, Foldable t, Functor t) => t a -> a
> sqx xs = sum (fmap (\x -> (x - mean) ^ 2) xs)
>  where
>   mean = sum xs / fromIntegral (length xs)
 




> mittelwerte :: (Fractional a, Enum a) => [a] -> [a]
> mittelwerte xs = helfer xs 0 1
>   where
>    helfer  [] _ _ = []
>    helfer  (x:xs) sum n = let newM= (fromIntegral (n - 1) * sum + x) / fromIntegral n
>                                     in newM : helfer  xs newM (n + 1)




> readBinary:: String -> Integer
> readBinary xs = aux 0 xs
>   where
>    aux result [] = result
>    aux result (x:xs)
>       | x == '1' = aux (result * 2 + 1) xs  
>       | x == '0' = aux (result * 2) xs      
>       | otherwise = error "Invalid character in binary string" 




> toOctalString :: (Show a, Integral a) => a -> String
> toOctalString 0 = "0"  
> toOctalString x = reverse (aux x)
>   where
>   aux 0 = ""
>   aux n = let (q, r) = n `quotRem` 8 
>           in  char r : aux q  
>   char r = toEnum (fromEnum '0' + fromIntegral r)  





> occurrences :: (Num a, Eq t) => [t] -> [(a, t)]
> occurrences xs = map (\x -> (fromIntegral (length (filter (== x) xs)), x)) (nub xs)





> anagram :: Eq a => [a] -> [[a]]
> anagram xs = [];