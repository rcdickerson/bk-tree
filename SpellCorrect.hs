module SpellCorrect
  where

import BKTree

-- From http://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Levenshtein_distance#Haskell
levDistance :: Metric String
levDistance sa sb = last $ foldl transform [0..length sa] sb 
   where 
     transform xs@(x:xs') c = scanl compute (x+1) (zip3 sa xs xs') 
       where 
         compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

buildFromDictFile :: String -> IO (BKTree String)
buildFromDictFile fileName = do
  dictContents <- readFile fileName
  return $ foldl bkInsert (mkBkTree levDistance) (lines dictContents)
