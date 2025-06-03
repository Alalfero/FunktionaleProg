



> module Cross where





> import Data.List





> data Direction = Horizontal|Vertical deriving (Eq,Show)





> changeDirection Horizontal = Vertical
> changeDirection Vertical = Horizontal





> data Grid a = Grid Int Int [Q a][[Maybe (Maybe Int,Border,a)]]
>   deriving (Eq,Show)





> data Border = None|Right|Bottom|RightBottom
>   deriving (Eq,Show)





> data Q a = Q Int (Int,Int) Direction String [a]
>   deriving (Eq,Show)





> --instance Show a => Show (Grid a) where
> showMe (Grid _ _ ws xss) = 
>  show ws ++"\n\n"
>    ++(concat
>      $ intersperse "\n" 
>      $ map (\xs-> concat
>                  $ map (\x->maybe "' '" (\(_,_,c)->show c) x) xs) xss)





> create :: Eq a => Int -> [(String,[a])] -> [Grid a]
> create w (x:xs) 
>   = map addNumbering
>     $ foldl (\rs y -> (selectBest$concat$map (insertWord y) rs))
>     [(firstWord x (newGrid w))] xs





> ex = create  10
>   [("Begrüßung","HALLO")
>   ,("Wo wir alle leben","WELT")
>   ,("Örtchen","LOKUS")
>   ,("Heizvorrichtug","OFEN")
>   ,("Zu beachten!","WICHTIG")
>   ,("Ganz Großartig","SUPER")
>   ,("Nahestehende doch nicht verwandr","FREUNDE")
>   ,("Antikes Volk und moderne Städter","ROEMER")
>   ,("Nicht unbedingt der Preis","WERT")
>   ,("In einem Land lebend","LANDSLEUTE")
>   ,("Kann auch schwere Arbeit sein, so zu konstruieren.","LEICHTBAU")
>   ,("Limes","GRENZE")
>   ,("Persönliches Fürwort","ICH")
>   ,("Pronomen","IHR")
>   ,("Da tappen manche falsch herum im Nebel","LEBEN")
>   ,("Oder doch lieber tun?","LASSEN")
>   ,("Fährt unter dem Pflaster","UBAHN")
>   ,("Weiblicher Artikel","DIE")]



writeFile "ex.tex"$toLaTeX $head ex





> class ToLaTeX a where
>   toLaTeX :: a -> String





> instance ToLaTeX Border where
>   toLaTeX None = ""
>   toLaTeX Cross.Right = "[r]"
>   toLaTeX Bottom = "[b]"
>   toLaTeX RightBottom = "[rb]"





> instance Show a => ToLaTeX (Grid a) where
>  toLaTeX (Grid w h ws gss) =
>    "\\begin{Puzzle}{"++show w++"}{"++show h++"}% \n"
>    ++latexGrid gss++"\\end{Puzzle}\n\n"
>    ++"\\paragraph*{Horizontal}~\\\\\n"
>    ++(concat $ map showQuestion hors)
>    ++"\\paragraph*{Vertikal}~\\\\\n"
>    ++(concat $ map showQuestion vers)  
>     where
>        latexGrid  = concat.(map latexRow)
>        latexRow rs = "|"++
>          (intercalate "|" 
>          $ map (maybe "*" 
>                 (\(a,b,c) -> ((maybe "[]" (\x->"["++show x++"]") a)
>                        ++toLaTeX b++[(show c!!1)]))) rs)
>           ++"|.\\\\\n"
>      
>        (ver,hor) = partition (\(Q _ _ d _ _)->d==Vertical) ws
>        hors = sortOn (\(Q nr _ _ _ _)->nr) hor 
>        vers = sortOn (\(Q nr _ _ _ _)->nr) ver 
>        showQuestion (Q nr _ _  q _)
>          = "{\\bfseries "++show nr++": }"++q++"\\\\\n"





> replaceAt :: Int -> a -> [a] -> [a]
> replaceAt i y xs  
>   | i < 0 || i > length xs = xs
>   | otherwise = take i xs ++ [y] ++ drop (i + 1) xs





> clusterBy :: (t -> t -> Bool) -> [t] -> [[t]]
> clusterBy _ [] = []
> clusterBy eq (x:xs) = go [x] xs
>  where
>    go acc [] = [reverse acc]
>    go acc@(a:_) (y:ys)
>      | eq a y    = go (y:acc) ys
>      | otherwise = reverse acc : go [y] ys



*Cross> clusterBy (==) "mississippi"
["m","iiii","ssss","pp"]



Cross> groupBy (==) "mississippi"
["m","i","ss","i","ss","i","pp","i"]





> newGrid :: Int -> Grid a
> newGrid w = Grid w w [] (replicate w (replicate w Nothing))





> firstWord :: (String,[a]) -> Grid a -> Grid a
> firstWord _ g = g




> transposeGrid :: Grid a -> Grid a
> transposeGrid g = g





> insertWordInLine
>   :: (Eq a1, Eq a2, Num a3) =>
>      [a2]
>      -> [Maybe (Maybe a1, Border, a2)]
>      -> [(a3, [Maybe (Maybe a1, Border, a2)])]
> insertWordInLine _ _ = []



*Cross> insertWordInLine "lok" [Just (Nothing,None,'h'),Just (Nothing,None,'a'
),Just (Nothing,None,'l'),Just (Nothing,None,'l'),Just (Nothing,None,'o'),Noth
ing,Nothing,Nothing,Nothing] 
[(3,[Just (Nothing,None,'h'),Just (Nothing,None,'a'),Just (Nothing,None,'l'),J
ust (Nothing,None,'l'),Just (Nothing,None,'o'),Just (Nothing,None,'k'),Nothing
,Nothing,Nothing]),(5,[Just (Nothing,None,'h'),Just (Nothing,None,'a'),Just (N
othing,None,'l'),Just (Nothing,None,'l'),Just (Nothing,None,'o'),Just (Nothing
,None,'l'),Just (Nothing,None,'o'),Just (Nothing,None,'k'),Nothing]),(6,[Just 
(Nothing,None,'h'),Just (Nothing,None,'a'),Just (Nothing,None,'l'),Just (Nothi
ng,None,'l'),Just (Nothing,None,'o'),Nothing,Just (Nothing,None,'l'),Just (Not
hing,None,'o'),Just (Nothing,None,'k')])]



*Cross> map (map (maybe ' ' (\(_,_,c)->c)))$map snd line
["hallok   ","hallolok ","hallo lok"]





> insertWord :: Eq a => (String, [a]) -> Grid a -> [Grid a]
> insertWord ws g = filter (not.hasOverlappings) 
>   (insertWordHorizontal ws g 
>   ++(map transposeGrid$insertWordHorizontal ws$transposeGrid g))





> insertWordHorizontal :: Eq a => 
>     (String, [a]) -> Grid a -> [Grid a]
> insertWordHorizontal  _ g = [g]





> hasOverlappings :: Grid a -> Bool
> hasOverlappings _ = False





> selectBest :: Eq a => [Grid a] -> [Grid a]
> selectBest gs = gs





> addNumbering :: Grid a -> Grid a
> addNumbering g = g

