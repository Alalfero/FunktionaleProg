



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
> clusterBy eq xs = go xs []
>   where
>     go [] _ = []
>     go (y:ys) seen
>       | any (eq y) seen = go ys seen
>       | otherwise =
>          let group = filter (eq y) xs
>           in group : go ys (y:seen)



*Cross> clusterBy (==) "mississippi"
["m","iiii","ssss","pp"]



Cross> groupBy (==) "mississippi"
["m","i","ss","i","ss","i","pp","i"]





> newGrid :: Int -> Grid a
> newGrid w = Grid w w [] (replicate w (replicate w Nothing))





> firstWord :: (String,[a]) -> Grid a -> Grid a
> firstWord (question, answer) (Grid w h _ gss) = 
>  Grid w h [q] newGr
>   where
>   row = h `div` 2
>   col = (w - length answer) `div` 2
>   q = Q 0 (row, col) Horizontal question answer
>   oldRow = gss !! row
>   newRow = take col oldRow
>               ++ map (\c -> Just (Nothing, None, c)) answer
>               ++ drop (col + length answer) oldRow
>   newGr = replaceAt row newRow gss




> transposeGrid :: Grid a -> Grid a
> transposeGrid (Grid w h qs cells) =
>  Grid h w qs' (transpose cells)
>  where
>    qs' = [Q nr (col, row) (changeDirection dir) frage antwort
>          | Q nr (row, col) dir frage antwort <- qs]





> insertWordInLine
>   :: (Eq a1, Eq a2, Num a3) =>
>      [a2]
>      -> [Maybe (Maybe a1, Border, a2)]
>      -> [(a3, [Maybe (Maybe a1, Border, a2)])]
> insertWordInLine word line =
>  [ (fromIntegral i, insertAt i word line)
>  | i <- [0 .. length line - length word]
>  , canInsertAt i word line
>  ]

> insertAt :: Int -> [a] -> [Maybe (Maybe n, Border, a)] -> [Maybe (Maybe n, Border, a)]
> insertAt i word line =
>  take i line ++ map (\c -> Just (Nothing, None, c)) word ++ drop (i + length word) line

> canInsertAt :: Eq a => Int -> [a] -> [Maybe (Maybe n, Border, a)] -> Bool
> canInsertAt i word line =
>   all match (zip word (drop i line))
>   where
>     match (w, Nothing) = True
>     match (w, Just (_, _, c)) = w == c




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
> insertWordHorizontal (frage,wort) (Grid w h qs gss) =
>   [ Grid w h (qs ++ [Q (length qs) (rowIx, posIx) Horizontal frage wort])
>       (replaceAt rowIx newRow gss)
>   | (rowIx, row) <- zip [0..] gss
>   , (posIx, newRow) <- insertWordInLine wort row
>   ]






> hasOverlappings :: Grid a -> Bool
> hasOverlappings (Grid _ _ _ rows) =
>   length positions /= length (nub positions)
>   where
>     positions =
>       [ (r, c)
>       | (r, row) <- zip [0..] rows
>       , (c, cell) <- zip [0..] row
>       , Just _ <- [cell]
>      ]






> selectBest :: Eq a => [Grid a] -> [Grid a]
> selectBest gs =
>   let scored = map (\g -> (sharedLetters g, g)) gs
>       maxScore = maximum (map fst scored)
>   in [g | (score, g) <- scored, score == maxScore]

> sharedLetters :: Eq a => Grid a -> Int
> sharedLetters (Grid _ _ _ rows) =
>   length   [ch1 |
>   row <- rows,
>   (i, c1) <- zip [0 .. ] row,
>   let rest = drop (i + 1) row,
>   Just (_, _, ch1) <- [c1],
>   any
>     (\ c2
>        -> case c2 of
>             Just (_, _, ch2) -> ch1 == ch2
>             _ -> False)
>     rest]





> addNumbering :: Grid a -> Grid a
> addNumbering (Grid w h qs cells) = Grid w h newQs newCells
>   where
>     numberedQs = zipWith (\n (Q _ pos dir q a) -> Q n pos dir q a) [0..] qs
>     newQs = numberedQs

>     newCells = foldl addQ cells numberedQs

>     addQ g (Q n (r, c) dir _ answer) =
>       let posList = case dir of
>                       Horizontal -> zip (repeat r) [c .. c + length answer - 1]
>                       Vertical   -> zip [r .. r + length answer - 1] (repeat c)
>           g1 = updateCell g (head posList) (\(_, b, ch) -> (Just n, b, ch))
>           g2 = updateCell g1 (last posList) (\(mi, b, ch) -> (mi, newBorder, ch))
>             where newBorder = case dir of
>                                 Horizontal -> Cross.Right
>                                 Vertical   -> Bottom
>       in g2

>     updateCell g (r, c) f =
>       let row = g !! r
>           cell = row !! c
>           newCell = case cell of
>                       Just (mi, b, ch) -> Just (f (mi, b, ch))
>                       Nothing -> error "Tried to update an empty cell"
>           newRow = replaceAt c newCell row
>       in replaceAt r newRow g


