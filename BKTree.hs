module BKTree
  where

import Data.List (intersperse)

type (Metric a) = a -> a -> Int

data BKNode a = Empty 
              | BKNode { bknValue :: a
                       , bknChildren :: [(Int, (BKNode a))]
                       }

data BKTree a = BKTree { bktMetric :: Metric a
                       , bktRoot   :: BKNode a  
                       }

instance Show a => Show (BKTree a) where
  show (BKTree _ root) = show root

instance Show a => Show (BKNode a) where
  show Empty = ""
  show (BKNode value children) = (show value) ++ childStr
    where 
      childStr = case children of 
                   [] -> ""
                   otherwise -> showChildren
      showChildren = "[" ++ (concat $ intersperse ", " $ 
                      map (show.snd) children) ++ "]"

insert :: (BKTree a) -> a -> (BKTree a)
insert (BKTree metric root) a = BKTree metric newRoot
  where newRoot = nInsert metric root a

nInsert :: (Metric a) -> (BKNode a) -> a -> (BKNode a)
nInsert _ Empty a = BKNode a []
nInsert metric (BKNode b children) a = 
  let distance = metric a b
  in case (Prelude.lookup distance children) of
       Nothing   -> BKNode b $ (distance, (BKNode a [])):children
       Just node -> BKNode b $ 
           addToAL children distance (nInsert metric node a)

-- From John Goerzen's Data.List.Utils package
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l
--

bkLookup :: (BKTree a) -> a -> Int -> [a]
bkLookup (BKTree metric root) a maxDist = filter withinDist results
  where
    withinDist x = (metric a x) <= maxDist
    results = nLookup 1 metric maxDist root

nLookup :: Int -> (Metric a) -> Int -> (BKNode a) -> [a]
nLookup _ _ _ Empty = []
nLookup n metric d (BKNode a children) = concat (matches:childMatches)
  where
    matches = between (d-n) (d+n) children 
    childMatches = map ((nLookup (n+1) metric d).snd) children

between :: Int -> Int -> [(Int, (BKNode a))] -> [a]
between low high list = map (bknValue.snd) $ filter (isBetween.fst) list
  where
    isBetween dist = dist >= low && dist <= high

testMetric :: Int -> Int -> Int
testMetric i1 i2 = abs $ i2 - i1