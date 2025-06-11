



> module Zustand where





> import Control.Monad.Trans.Maybe
> import Control.Monad.Trans.Class         
> import Control.Monad
> import Control.Monad.IO.Class





> newtype Anweisung st r =  Z (st -> (st,r))





> setvar :: String -> Integer -> Anweisung [(String,Integer)] Integer
> setvar x v = Z (\env -> ((x,v):env,v))





> getvar :: String -> Anweisung [(String,Integer)] (Maybe Integer)
> getvar x = Z (\env -> (env,lookup x env))



class C{
  static int f(){
    var x = 17;
    var y = 4;
    var z = 2;
    return (x+y)*z;
  }
}



> ex1 = 
>    let 
>      Z anw1 = setvar "x" 17
>      Z anw2 = setvar "y" 4
>      Z anw3 = setvar "z" 2
>      Z anw4 = getvar "x"
>      Z anw5 = getvar "y"
>      Z anw6 = getvar "z"





>      (st1,_) = anw1 []
>      (st2,_) = anw2 st1
>      (st3,_) = anw3 st2
>      (st4,Just x) = anw4 st3
>      (st5,Just y) = anw5 st4
>      (st6,Just z) = anw6 st5
>    in (st6,(x+y)*z) 





> undDann:: (Anweisung st r1) -> (Anweisung st r2) -> (Anweisung st r2)
> (Z anw1)  `undDann` (Z anw2)
>   = Z (\s -> let (s1,r1) = anw1 s
>                  (s2,r2) = anw2 s1
>               in (s2,r2) )





> ex2 = 
>    let 
>      Z anw123 = 
>        setvar "x" 17  `undDann`
>        setvar "y" 4   `undDann`
>        setvar "z" 2
>      Z anw4 = getvar "x"
>      Z anw5 = getvar "y"
>      Z anw6 = getvar "z"
>      (st3,_) = anw123 []
>      (st4,Just x) = anw4 st3
>      (st5,Just y) = anw5 st4
>      (st6,Just z) = anw6 st5
>    in (st6,(x+y)*z) 





> undMit::(Anweisung st r1)->(r1->Anweisung st r2)->(Anweisung st r2)
> (Z st1)  `undMit` fs
>   = Z (\s -> let (s1,r1) = st1 s
>                  (Z anw2) = fs r1
>               in anw2 s1)





> ergebnis :: r -> Anweisung st r
> ergebnis r = Z (\st -> (st,r))





> ex3 = 
>    let 
>      Z anws = 
>        setvar "x" 17 `undDann`  
>        setvar "y" 4  `undDann` 
>        setvar "z" 2  `undDann` 
>        getvar "x"    `undMit` \(Just x) -> 
>        getvar "y"    `undMit` \(Just y) -> 
>        getvar "z"    `undMit` \(Just z) -> 
>        ergebnis ((x+y)*z)  
>   in anws []





> infixl 1 -->
> infixl 1 ->-

> (-->) = undDann
> (->-) = undMit





> ex4 = let 
>   Z anws = 
>     setvar "x" 17 -->
>     setvar "y" 4  -->
>     setvar "z" 2  -->
>     getvar "x"    ->- \(Just x) -> 
>     getvar "y"    ->- \(Just y) -> 
>     getvar "z"    ->- \(Just z) -> 
>     ergebnis ((x+y)*z)  
>  in anws []





> (=:) = setvar





> x ! () = getvar x
 




> ex5 = let 
>   Z anws = 
>     "x" =: 17 -->
>     "y" =: 4  -->
>     "z" =: 2  -->
>     "x"!()    ->- \(Just x) -> 
>     "y"!()    ->- \(Just y) -> 
>     "z"!()    ->- \(Just z) -> 
>     ergebnis ((x+y)*z)  
>  in anws []




    
> run (Z stm) = stm []





> ex6 =
>   "x" =: 17 -->
>   "y" =: 4  -->
>   "z" =: 2  -->
>   "x"!()    ->- \(Just x) -> 
>   "y"!()    ->- \(Just y) -> 
>   "z"!()    ->- \(Just z) -> 
>   ergebnis ((x+y)*z)  





> instance Monad (Anweisung st) where
>   -- (>>)   = undDann 
>   -- return = ergebnis
>   (>>=)  = undMit





> instance Functor (Anweisung st) where
>   fmap f (Z st) = Z (\s -> let (s1,r1) = st s in (s1,f r1))





> instance Applicative (Anweisung st) where
>   pure  = ergebnis
>   (Z gf) <*> (Z ga)
>     = Z (\s -> let (s1,f) = gf s
>                    (s2,a) = ga s1
>                in (s2,f a))





> ex7 =  
>   "x" =: 17  >>
>   "y" =: 4   >>
>   "z" =: 2   >>
>   "x"!()     >>= \(Just x)->
>   "y"!()     >>= \(Just y)->
>   "z"!()     >>= \(Just z)->
>   return ((x+y)*z)





> ex8 = do 
>   "x" =: 17
>   "y" =: 4 
>   "z" =: 2 
>   mx <- "x"!()
>   let (Just x) = mx
>   my <- "y"!()
>   let (Just y) = my
>   mz <- "z"!()
>   let (Just z) = mz
>   return ((x+y)*z)





> eval1 env = do
>   x <- lookup "x" env
>   y <- lookup "y" env
>   z <- lookup "z" env
>   return ((x+y)*z)



*Zustand> eval1 [("x",17),("y",4),("z",2)]
Just 42
*Zustand> eval1 [("x",17),("y",4)]
Nothing
*Zustand> eval1 [("x",17),("z",2)]
Nothing





> bsp1 = do
>   x <- lookup "x" [("y",42),("x",17)]
>   y <- lookup "y" [("y",42),("x",17)]
>   z <- lookup "z" [("y",42),("x",17)]
>   return (x+y)

*Zustand> bsp1
Nothing



> ex9 = do 
>   "x" =: 17
>   "y" =: 4 
>   "z" =: 2 
>   mx <- "x"!()
>   my <- "y"!()
>   mz <- "z"!()
>   return $ do 
>     x<-mx
>     y<-my
>     z<-mz
>     return ((x+y)*z)





newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }





> ex10 :: MaybeT (Anweisung [(String,Integer)]) Integer
> ex10 = do 
>   MaybeT$fmap Just ("x" =: 17)
>   MaybeT$fmap Just ("y" =: 4) 
>   MaybeT$fmap Just ("z" =: 2) 
>   xm <- MaybeT ("x"!())
>   ym <- MaybeT ("y"!())
>   zm <- MaybeT ("z"!())
>   return  ((xm+ym)*zm)


*Zustand> run (runMaybeT ex10) 
([("z",2),("y",4),("x",17)],Just 42)




> ex11 :: MaybeT (Anweisung [(String,Integer)]) Integer
> ex11 = do 
>   lift ("x" =: 17)
>   lift ("y" =: 4) 
>   lift ("z" =: 2) 
>   xm <- MaybeT ("x"!())
>   ym <- MaybeT ("y"!())
>   zm <- MaybeT ("z"!())
>   return  ((xm+ym)*zm)





> bsp2 = do
>   print "geben sie eine Zahl ein"
>   i <- (readLn::IO Integer)
>   print "das Quadrat ist"
>   print (i*i)
>   xs <- readFile "Zustand.lhs"
>   writeFile "newZust.hs"  xs





> bsp3 = do
>   x <- [1,2,3,4]
>   y <- [5,6,7]
>   return (x,y)
                 
Zustand> bsp3
[(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7),(4,5),(4,6),(4,7)]

*Zustand> [(x,y)|x<-[1,2,3,4],y<-[5,6,7]]
[(1,5),(1,6),(1,7),(2,5),(2,6),(2,7),(3,5),(3,6),(3,7),(4,5),(4,6),(4,7)]
*Zustand> bsp3 == [(x,y)|x<-[1,2,3,4],y<-[5,6,7]]
True

*Zustand> [(x,y)|x<-[1,2,3,4],y<-[5,6,7],x`mod`2==0]
[(2,5),(2,6),(2,7),(4,5),(4,6),(4,7)]




> bsp4 = do
>   x <- [1,2,3,4]
>   y <- [5,6,7]
>   guard (x`mod`2==0)
>   return (x,y)
                 

*Zustand> bsp4
[(2,5),(2,6),(2,7),(4,5),(4,6),(4,7)]




> data Imperator = 
>   Lese String
>  |Drucke Imperator
>  |Zuweisung String Imperator
>  |Literal Integer
>  |Var String
>  |Arith Imperator (Integer->Integer->Integer) Imperator
>  |While Imperator Imperator
>  |Sequenz [Imperator]





> factorial = Sequenz
>   [Lese "x"
>   ,Zuweisung "r" (Literal 1)
>   ,While
>      (Var "x")
>      (Sequenz
>        [Zuweisung "r" (Arith (Var "r")(*)(Var "x"))
>        ,Zuweisung "x" (Arith (Var "x")(-)(Literal 1))
>        ]
>      )
>   ,Drucke (Var "r") 
>   ]





> newtype AnweisungT st m r
>    = AnweisungT {runAnweisungT::(st -> m (st, r))}





> instance Monad m =>  Functor (AnweisungT st m) where
>   fmap f (AnweisungT anw)
>     = AnweisungT (\x-> fmap (\(s',a)->(s', f a)) (anw x)) 





> instance Monad m =>  Monad (AnweisungT st m) where
>   (AnweisungT anw1) >>= k  = AnweisungT $ \ s -> do
>      ~(s',a) <- anw1 s
>      let (AnweisungT anw2) = (k a)
>      anw2 s'
>
>   -- fail str = AnweisungT $ \ _ -> fail str





> instance (Functor m, Monad m) => Applicative (AnweisungT st m) where
>   pure a = AnweisungT $ \ s -> return (s, a)
>
>   AnweisungT mf <*> AnweisungT mx = AnweisungT $ \ s -> do
>     ~(s', f) <- mf s
>     ~(s'', x) <- mx s'
>     return (s'', f x)

>   m *> k = m >>= \_ -> k





> instance MonadTrans (AnweisungT st) where
>   lift m = AnweisungT $ \ st -> do
>     a <- m
>     return (st, a)




> instance (MonadIO m) => MonadIO (AnweisungT st m) where
>  liftIO io = lift  (liftIO io)





> liftAnweisung (Z f) = AnweisungT (\st -> return (f st))





> execute :: Imperator
>    ->  MaybeT (AnweisungT [(String, Integer)] IO) Integer

*Zustand> (runAnweisungT.runMaybeT.execute) (Literal 42) [] 
([],Just 42)



> execute (Literal n) = return n





> execute (Var v) =MaybeT $ liftAnweisung (getvar v)





> execute (Drucke imp) = do
>   val <- execute imp
>   liftIO $ print val
>   return val





> execute (Zuweisung v imp) = do
>   val <- execute imp
>   lift $ liftAnweisung (setvar v val)
>   return val





> execute (Lese v) = do
>   val <- liftIO readLn
>   lift $ liftAnweisung (setvar v val)
>   return val



> execute (Arith l op r) = do
>   val <- execute l
>   val2<- execute r
>   return  (op val val2) 



> execute w@(While cond body) = do
>   c <- execute cond
>   if c /= 0
>       then do
>           _ <- execute body
>           execute w
>   else return 0




> execute (Sequenz []) = return 0
> execute (Sequenz [i]) = execute i
> execute (Sequenz (i:is)) = do
>   _ <- execute i
>   execute (Sequenz is)


