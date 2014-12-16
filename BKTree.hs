-- A simple BK-Tree implementation.
--
-- Copyright (c) 2014 Rob Dickerson
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

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
  where newRoot = insert metric root ins

insert :: (Metric a) -> (BKNode a) -> a -> (BKNode a)
insert _ Empty a = BKNode a 0 []
insert metric (BKNode val dist children) ins = BKNode val dist newChildren
  where
    distance = metric ins val
    newChildren = case (lookupChildWithDist children distance) of
       Nothing   -> (BKNode ins distance []):children
       Just node -> (insert metric node ins):
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
