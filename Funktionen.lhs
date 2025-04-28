



> module Funktionen where





> import Data.Ratio





> square x = x * x





> i x = x





> x42 = (17 + 4) * 2





> bot = bot

<A> A bot(){return bot();}



> addTwice x y = 2 * (x + y)





> k x _ = x





> doppel x = x + x
> quadrat  x = x * x





> nach f g x =  f (g x)





> qd = nach quadrat doppel





> qd2 x = nach quadrat doppel x





> s x y z =  x z (y z)





> qd3 =  nach (\x->x*x) (\x->x+x) 





> qd4 = \x -> nach quadrat doppel x





> qd5 = quadrat `nach` doppel





> f -<- g  = \x -> f (g x)





> qd6 = quadrat-<-doppel




(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)




> qd7 = quadrat.doppel



 

f $ x =  f x





> qd8 x = quadrat$doppel x



 
infixr 9  .
infixr 0  $


 
infixl 6 +
infixl 7 *




> absolute1 x 
>   |x < 0 = (-1)*x
>   |otherwise =  x





> signum x 
>   |x > 0     = 1
>   |x < 0     = (-1)
>   |otherwise =  0





> absolute2 x = if x < 0 then (-1)*x else x





> wenn p a1 a2 = if p then a1 else a2





> zahlenworte 0 = "null"
> zahlenworte 1 = "eins"
> zahlenworte 2 = "zwei"
> zahlenworte 3 = "drei"
> zahlenworte 4 = "vier"
> zahlenworte 5 = "fünf"
> zahlenworte x 
>   |x>0 = "viele"
>   |otherwise = "negativ"





> f1 x y k =  p x + p y
>   where
>     p z = 3*z^k-5*z^2 





> f2 x y k =  
>   let 
>     p z = 3*z^k-5*z^2 
>   in
>    p x + p y



 

> (/%) :: Integral b => b -> b -> (b, b)
> x /% y = (x `div` y, x `mod` y)



 

> f3(x,y) = 2 * x^2 + y^3





> factorial :: (Num a, Eq a) => a -> a
> factorial x = if x == 0 || x == 1 then 1 else x * factorial(x-1)




> fib :: (Ord a, Num a) => a -> a
> fib x
>   | x == 1 = 1
>   | x == 2 = 1
>   | otherwise = fib (x - 1) + fib (x - 2)





> fib2 :: (Integral b1, Integral b2) => b2 -> b1
> fib2 x =  round $ (((1 + sqrt 5) / 2) ** fromIntegral x - ((1 - sqrt 5) / 2) ** fromIntegral x) / sqrt 5






> quersumme:: (Ord a, Integral a) => a -> a
> quersumme x
>   | x < 10    = x
>   | otherwise = quersumme (x `mod` 10 + quersumme (x `div` 10))




> a :: (Num a, Num t, Eq a, Eq t) => a -> t -> t
> a n m 
>   | n == 0 = m + 1
>   | m == 0 = a (n-1) 1
>   | otherwise = a (n-1) (a n (m-1))



> heron :: (Ord a, Num a, Fractional p) => a -> p -> p
> heron n a 
>   | n < 0 = a
>   | otherwise = (heron (n-1) a + ( a / heron (n-1) a) ) / 2





> gcdExt :: Integer -> Integer -> (Integer,Integer,Integer)
> gcdExt a b 
>   | b == 0 = (a, 1, 0)
>   | otherwise = 
>     let (d', s', t') = gcdExt b (a `mod` b)
>     in (d', t', s' - t' * (a `div` b))





> (#) :: (Integral a1, Num a2) => a2 -> a1 -> a2
> x#0 = 1





> kniffel :: Int -> Rational
> kniffel n = prob n 5 5
>   where 
>     prob n missing roll = 0





> summeNbis
>   :: (Ord t1, Num a, Num t1) =>
>      t1 -> t1 -> (t1 -> t2 -> a) -> t2 -> a
> summeNbis start ende term = term start 





> summe0bis = summeNbis 0 

> summe1bis = summeNbis 1 





> exTerm :: Integral a => a -> a -> Ratio a
> exTerm n = \x -> 0





> eHoch :: Integer -> Rational 
> eHoch = summe0bis 100 exTerm





> eHoch2 :: Integer -> Double 
> eHoch2 = fromRational.eHoch





> lnTerm ::  Integral a => a -> Ratio a -> Ratio a
> lnTerm n = \x -> 0





> ln = fromRational.summe1bis 10 lnTerm





> sinTerm :: Integer -> Rational -> Rational
> sinTerm n = \x -> 0





> facR = toRational.factorial





> sinus :: Double -> Double
> sinus = fromRational.summe0bis 10 sinTerm.toRational





> cosTerm :: Integer -> Rational -> Rational
> cosTerm n = \x -> 0





> cosinus :: Double -> Double
> cosinus = fromRational.summe0bis 10 cosTerm.toRational

