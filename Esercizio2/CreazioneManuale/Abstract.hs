module Abstract where

data Tree a = Void | Node a [Tree a]
   deriving (Eq, Ord, Show)

treefoldr  :: (Eq a,Show a) => (a->b->c)->c->(c->b->b)->b->Tree a->c
treefoldr f zf g zg Void = zf
treefoldr f zf g zg (Node x xs) = f x $ foldr (g . treefoldr f zf g zg) zg xs

height t = treefoldr  node (-1) max (-1) t
    where
         node _ = (1+)

adjust  :: (Integral a, Fractional b, Show a) => Tree a -> Tree b
adjust t = fst $ treefoldr  aggr1 (Void ,  -1) aggr2  (0,[]) t
    where
         aggr1 x (h,ts) = (Node (( fromIntegral x) / (fromIntegral $ h+1)) ts ,h+1)
         aggr2 (t,h) (hm ,ts) = (max h hm , t:ts)
