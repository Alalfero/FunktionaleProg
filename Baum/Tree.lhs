



nub :: Eq a => [a] -> [a] 





union :: Eq a => [a] -> [a] -> [a] 
intersect :: Eq a => [a] -> [a] -> [a]





> module Tree where





> data Tree a = 



class Tree<A extends Comparable<? super A>>{




>   Empty



  boolean isEmpty = true;
  Tree(){}



>   |Branch (Tree a) a (Tree a)



  Tree<A> left = null;
  A element = null;
  Tree<A>  right = null;
  private Tree(Tree<A> l,A e,Tree<A> r){
    left = l;
    element = e;
    right = r;
    isEmpty = false;
  }




>     deriving (Show)



  long size(){
    if (isEmpty) return 0;
    return 1 + left.size() + right.size();
  }


> size :: Num a => Tree t -> a
> size Empty = 0



> size (Branch left element right) = 1+size left+size right




> add :: Ord a => a -> Tree a -> Tree a
> add x Empty = Branch Empty x Empty
> add x (Branch left element right) 
>   | x < element = Branch (add x left) element right
>   | x > element = Branch left element (add x right)
>   | otherwise = Branch left element right





  void add(A e){
    if (isEmpty) {
      left = new Tree<>();
      right = new Tree<>();
      element = e;
    }else if (e.compareTo(element)<0){
      left.add(e);
    }else {
      right.add(e);
    }
  }



> infixr 5 +>
> (+>)  el t = add el t

00002BinTree$ ghci solution/Tree.lhs 
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Tree             ( solution/Tree.lhs, interpreted )
Ok, one module loaded.
*Tree> let t1 = 17 +> 42 +> -1 +> 100 +> 576576576 +> Empty
*Tree> t1
Branch (Branch (Branch Empty (-1) (Branch (Branch Empty 17 Empty) 42 Empty)) 
100 Empty) 576576576 Empty
*Tree> size t1
5
*Tree> 



> contains :: Ord a => a -> Tree a -> Bool
> contains _ Empty = False
> contains x (Branch left element right)
>   | x == element = True
>   | x < element = contains x left
>   | otherwise = contains x right



  boolean contains(A e){
    if (isEmpty) return false;
    if (e.compareTo(element)==0) return true;
    if (e.compareTo(element)<0) return left.contains(e);
    return right.contains(e);
  }



> remove :: Ord a => a -> Tree a -> Tree a
> remove _ Empty = Empty
> remove x (Branch left element right)
>   | x < element = Branch (remove x left) element right
>   | x > element = Branch left element (remove x right)
>   | otherwise = rebuild left right
>   where 
>     rebuild Empty r = r
>     rebuild l Empty = l
>     rebuild (Branch l2 e2 r2) r = Branch l2 e2 (rebuild r2 r)



  void remove(A e){
    if (isEmpty) return;
    if (e.compareTo(element)<0) left.remove(e);
    else if (e.compareTo(element)>0) right.remove(e);
    else rebuild(left,right);
  }

  private void rebuild(Tree<A> l,Tree<A> r){
    if (l.isEmpty) {
      left = r.left;
      element = r.element;
      right = r.right;
      isEmpty = r.isEmpty;
    }else if (r.isEmpty) {
      left = l.left;
      element = l.element;
      right = l.right;
      isEmpty = l.isEmpty;
    }else {
      element = l.element;
      left = l.left;
      right.rebuild(l.right,r);
    }
  }




> infixr 5 >-          
> (>-) t el = remove el t





> inorder :: Tree t -> [t]
> inorder Empty = []
> inorder (Branch left element right) = inorder left ++ [element] ++ inorder right



  java.util.List<A> inorder(){
    var result = new java.util.ArrayList<A>();
    inorder(result);
    return result;
  }
  void inorder(java.util.List<A> result){
    if (isEmpty) return;
    left.inorder(result);
    result.add(element);
    right.inorder(result);
  }



> reduce :: Ord t => t1 -> (t1 -> t -> t1) -> Tree t -> t1
> reduce x _ Empty = x
> reduce x f (Branch left element right) = reduce (f (reduce x f left) element) f right



  <B> B reduce(B start,java.util.function.BiFunction<B,A,B> f){
    if (isEmpty) return start;

    remove(element);
    return reduce(f.apply(start,element),f);
  }
} //end of class Tree



> infixl 5 \/
> (\/) :: Ord a => Tree a -> Tree a -> Tree a
> xs \/ ys = reduce xs (flip add) ys





> infixl 5 /\
> (/\) :: Ord a => Tree a -> Tree a -> Tree a
> xs /\ ys = reduce Empty (\acc x -> if contains x ys then add x acc else acc) xs





> instance (Ord a) => Eq (Tree a) where
>   xs == ys = size xs == size ys && size (xs /\ ys) == size xs





> tmap :: (Ord a,Ord b) => (a -> b) -> Tree a -> Tree b
> tmap f xs = reduce Empty (\acc x -> add (f x) acc) xs





s1 :: Tree Integer
> s1 = 56 +> 5 +> 67 +> 17 +> 4 
>       +> -576 +> 42 +> 1000000078979887657 +> Empty





> t2  = "hallo" +> "welt" +> "witzebitzelbritz"
>        +> "pandudel" +> "sowieso" +> "klaro" +> Empty





> summe = reduce 0 (+) 
> 
> produkt = reduce 1 (*) 





> mappe _ Empty = Empty
> mappe f (Branch l el r) = Branch (mappe f l)(f el)(mappe f r)





> mapReduce f s op Empty = s
> mapReduce f s op (Branch l el r)   
>   = (f el) `op`  (mapReduce f s op l) `op` (mapReduce f s op r)





> mapReduce2 f s op tree = reduce s op $ mappe f tree





> size2 = \t -> summe $ mappe (\x->1) t




 
> size3 = mapReduce (\_->1) 0 (+)





> containsWithProp p tree = reduce False (||) $ mappe (\x-> p x) tree





> haslongelement :: Tree [a] -> Bool
> haslongelement = containsWithProp (\x->length x > 10)

