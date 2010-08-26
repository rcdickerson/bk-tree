module BKTree (Metric,
               BKTree,
               mkBkTree,
               bkInsert,
               bkLookup)
               
  where

import Data.List (intersperse)

type (Metric a) = a -> a -> Int

data BKNode a = Empty 
              | BKNode { bknValue :: a
                       , bknDistance :: Int
                       , bknChildren :: [(BKNode a)]
                       }

data BKTree a = BKTree { bktMetric :: Metric a
                       , bktRoot   :: BKNode a  
                       }

mkBkTree :: (Metric a) -> (BKTree a)
mkBkTree metric = BKTree metric Empty

instance Show a => Show (BKTree a) where
  show (BKTree _ root) = show root

instance Show a => Show (BKNode a) where
  show Empty = ""
  show (BKNode value dist children) = (show value) ++ childStr
    where 
      childStr = case children of 
                   [] -> ""
                   otherwise -> showChildren
      showChildren = "[" ++ (concat $ intersperse ", " $ 
                      map show children) ++ "]"

bkInsert :: (BKTree a) -> a -> (BKTree a)
bkInsert (BKTree metric root) ins = BKTree metric newRoot
  where newRoot = nInsert metric root ins

nInsert :: (Metric a) -> (BKNode a) -> a -> (BKNode a)
nInsert _ Empty a = BKNode a 0 []
nInsert metric (BKNode val dist children) ins = BKNode val dist newChildren
  where
    distance = metric ins val
    newChildren = case (lookupChildWithDist children distance) of
       Nothing   -> (BKNode ins distance []):children
       Just node -> (nInsert metric node ins):
               (filter (\n->(bknDistance n) /= distance) children)

lookupChildWithDist :: [(BKNode a)] -> Int -> Maybe (BKNode a)
lookupChildWithDist [] _ = Nothing
lookupChildWithDist (n:ns) dist = if (bknDistance n) == dist
                                    then Just n
                                    else lookupChildWithDist ns dist

bkLookup :: (BKTree a) -> Int -> a -> [a]
bkLookup (BKTree metric root) maxDist target =
  let
    rootDist = metric target (bknValue root)
    lowDist = rootDist - maxDist
    highDist = rootDist + maxDist
    
    inDistance node = (bknDistance node) >= lowDist 
                   && (bknDistance node) <= highDist
    childrenInDist = filter inDistance (bknChildren root)  
 
    lookupOnChild child = bkLookup (BKTree metric child) maxDist target

    childLookups = concat $ map lookupOnChild childrenInDist
  in
    if (rootDist <= maxDist)
      then (bknValue root) : childLookups
      else childLookups

testMetric :: Int -> Int -> Int
testMetric i1 i2 = abs $ i2 - i1