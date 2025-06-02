



> module OurParserLib where
> import Data.Char
> import Data.Maybe





> type ParsResult token result = (result,[token])  
> type Parser token result = [token] -> [ParsResult token result]





> satisfy :: (t -> Bool) -> Parser t t
> satisfy p  [] = []
> satisfy p (x:xs)
>  |p x = [(x,xs)]
>  |otherwise = []





> terminal :: Eq t => t ->  Parser t t
> terminal t = satisfy ((==) t)





> epsilon ::  Parser t [t]
> epsilon xs = [([],xs)]





> infixl 9 +-
> infixl 9 +--
> infix  6 <<-
> infixl 3 !|+
> infixl 3 |+





> (+-) :: Parser t r1 -> Parser t r2 -> Parser t (r1,r2)
> (+-) p1 p2 = \xs -> [((r1,r2),ts2)| (r1,ts1)<-p1 xs, (r2,ts2)<-p2 ts1]





> (|+) :: Parser t r -> Parser t r -> Parser t r
> (|+) p1 p2 xs = p1 xs ++ p2 xs





> (!|+) :: Parser t r -> Parser t r -> Parser t r
> (!|+) p1 p2 xs 
>   |null p1res =  p2 xs
>   |otherwise = p1res
>   where p1res = p1 xs





> (<<-) :: Parser t r1 -> (r1 -> r2) -> Parser t r2
> (<<-) p f = \xs ->  [(f result,rest) |(result,rest) <-  p xs] 





> optional p
>   =     p       <<- (\x-> [x])
>     !|+ epsilon <<- (\_-> [])





> zeroToN :: Parser t r -> Parser t [r] 
> zeroToN  = zeroToN2 []
>   where
>     zeroToN2 acc p xs
>      | null res1 = [(reverse  acc,xs)]
>      | otherwise = zeroToN2 (r1: acc) p rest
>       where
>        res1 = p xs
>        (r1,rest) = head res1





> oneToN :: Parser t r -> Parser t [r] 
> oneToN p = p +- zeroToN p <<- \(x,xs)-> (x:xs)





> ignoreSpace :: Parser Char r -> Parser Char r
> ignoreSpace p = \xs -> p (dropWhile isSpace xs) 





> (+--) p1 p2 = p1 +- ignoreSpace p2





> rep p  = zeroToN (ignoreSpace p)





> filtere :: (r -> Bool) -> Parser t r -> Parser t r
> filtere  pred p = \xs ->  filter (\(r,_) -> pred r) (p xs)





> data Prog = Prog [Fundef] Expr
>   deriving (Show,Eq)  





> data Fundef = Fun String [String] [Statement]
>   deriving (Show,Eq)  





> data Statement = Simple Expr
>  |While Expr [Statement]
>  |Assignment String Expr
>   deriving (Show,Eq)  





> data Expr =
>   Number Integer
>  |Variable String
>  |FunCall String [Expr]
>  |BinOp Expr Operator Expr
>  |IfExpr Expr Expr Expr
>   deriving (Eq,Show)





> data Operator = OR|AND|OEQ|NEQ|LE|GE|OLT|OGT|ADD|SUB|MULT|DIV|MOD
>   deriving (Eq,Show)





> ident = oneToN (satisfy isLetter)





> number = oneToN (satisfy isDigit) <<- \xs -> (read xs)::Integer





> keyword xs
>   = (foldl (\p1 p2 -> p1 +- p2 <<- (\_ -> []))
>            epsilon
>            (map terminal xs))
>      <<- \_ -> xs





> expr :: Parser Char Expr
> expr = ifExpr !|+ booleanExpr 





> ifExpr =
>   keyword "if"   +-- expr +--
>   keyword "then" +-- expr +--
>   keyword "else" +-- expr
>     <<- (\(((((_,c),_),a1),_),a2) -> IfExpr c a1 a2)





> booleanExpr :: Parser Char Expr
> booleanExpr = compareExpr +-- (rep (booleanOperator+--compareExpr))
>    <<- (\(x,os) -> mkOpEx x os)





> booleanOperator = pOp [("&&",AND),("||",OR)]





> pOp ops = foldl1 (!|+) [keyword s <<- \_-> op |(s,op)<-ops]





> mkOpEx e = foldl (\e1 (op,e2)-> BinOp e1 op e2) e


> compareOperator = pOp [("==", OEQ),("/=",NEQ),("<=" ,LE),(">=" ,GE),("<" ,OLT),(">" ,OGT)]


> compareExpr :: Parser Char Expr
> compareExpr = 
>   addExpr +-- optional (compareOperator +-- compareExpr)
>   <<-(\(x,l) -> case l of 
>                       [] -> x
>                       [(op,y)] -> BinOp x op y) 





> addExpr :: Parser Char Expr
> addExpr = 
>   multExpr +-- rep (addOperator+--multExpr)
>   <<- (\(x,os) -> mkOpEx x os)

> addOperator = pOp [("+" ,ADD),("-" ,SUB)]





> multExpr :: Parser Char Expr
> multExpr = 
>    atom +-- rep (multOperator+--atom)
>   <<- (\(x,os) -> mkOpEx x os)

> multOperator = pOp [("*" ,MULT),("/" ,DIV),("%" ,MOD)]





> atom :: Parser Char Expr
> atom = (terminal '(' +-- expr +-- terminal ')' <<- \ ((a,b),c) -> b)
>   !|+ varOrFunCall
>   !|+ number <<- (\x -> Number x)




> varOrFunCall :: Parser Char Expr
> varOrFunCall = ident +-- optional (terminal '(' +-- optional args +-- terminal ')') 
>   <<- \((i,list) -> case list of 
>                       [] -> Variable i
>                       [((a,b),c)] -> FunCall i concat b )
> args = expr +-- rep (terminal ',' +-- expr) <<- \(a,b) ->b <<- (\(e,es) -> e:es)



> stat :: Parser Char Statement
> stat = whileStat
>    !|+ assignment +--terminal ';' <<- fst
>    !|+ simpleExpr +--terminal ';' <<- fst





> assignment :: Parser Char Statement
> assignment = ident +-- keyword ":=" +-- expr <<- (\((v,_),e) -> Assignment v e)





> body :: Parser Char [Statement]
> body = expr <<- (\e -> [Simple e])
>   !|+ terminal '{' +-- rep stat +-- terminal '}' <<-(\((_,sts),_)->sts)




> whileStat :: Parser Char Statement
> whileStat = keyword "while" +-- terminal '(' +-- expr +-- terminal ')' +--body 
>   <<- (\((((_,_),e),_),bs)-> While e bs)





> simpleExpr :: Parser Char Statement
> simpleExpr = expr <<- Simple





> fundef :: Parser Char Fundef
> fundef = keyford "fun" +-- ident +-- terminal '(' +-- vars +-- terminal ')' +-- terminal '=' +-- body
>   <<- (\((((((_,f),_),as),_),_),b) -> Fun f as b)
> vars = optional (ident +-- rep (terminal) ',' +-- ident <<- snd)
>   <<- (\(v,vs) -> v:vs) <<- (concat)




> prog :: Parser Char Prog
> prog = \xs ->[] 





> type VarName = String
> type Env = [(VarName,Integer)]





> getOp :: Operator -> Integer -> Integer -> Integer
> getOp ADD  = (+)
> getOp SUB  = (-)
> getOp MULT = (*)
> getOp DIV  = div
> getOp MOD  = mod





> getOp OEQ  = \x y -> toInteger$fromEnum (x==y)
> getOp NEQ  = \x y -> toInteger$fromEnum (x/=y)
> getOp GE   = \x y -> toInteger$fromEnum (x>=y)
> getOp LE   = \x y -> toInteger$fromEnum (x<=y)
> getOp OLT  = \x y -> toInteger$fromEnum (x<y)
> getOp OGT  = \x y -> toInteger$fromEnum (x>y)
> getOp AND  = \x y -> toInteger$fromEnum
>    ((toEnum$fromIntegral x)&&(toEnum$fromIntegral y))
> getOp OR   = \x y -> toInteger$fromEnum
>    ((toEnum.fromIntegral) x||(toEnum.fromIntegral) y)





> getFun :: VarName -> [Fundef] -> Fundef
> getFun n [] = error ("function not defined "++n)
> getFun n (f@(Fun fn args body):fs)
>   |n==fn = f
>   |otherwise = getFun n fs





> eval :: Env -> [Fundef] -> Expr -> Integer




> eval _ _ (Number i) = i





> eval env _ (Variable s) = fromJust $lookup s env 





> eval env fs (IfExpr c a1 a2) = 
>  if eval env fs c > 0 then eval env fs a1 else eval env fs a2

> eval env fs (BinOp left op right) = 
>   getOp op  (eval env fs left) (eval env fs right)

> eval env fs (FunCall n args) = 
>   case getFun n fs of
>   Fun _ params body -> runStats (zip params (map (eval env fs)args)) fs body






> run :: Env -> [Fundef] -> Statement -> (Env,Integer)





> run env fs (Simple e) = (env,eval env fs e)





> run env fs (Assignment v e) = ((v,r):env,r)
>   where r = eval env fs e





> run env fs w@(While c body) = (env,0)





> runStats env fs [stat] = run env fs stat
> runStats env fs (st:sts) = runStats (fst$run env fs st) fs sts 





> runProg (Prog fs e) = eval [] fs e

