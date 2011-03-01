-- Copyright © 2011 Bart Massey
module CSV (readCSV, showCSV)
where

import Data.List

data S = SW | SX | SQ | SDQ

data C = CX Char | CCO | CNL | CN deriving (Eq, Show)

label :: String -> [C]
label csv =
  let (s', cs) = mapAccumL next SW csv in
  let filtered = filter (/= CN) cs in
  trail filtered
  where
    trail cs =
      case last cs of
        CNL -> cs
        _ -> cs ++ [CNL]
    next SW  ' '  = (SW, CN)
    next SW  '\t' = (SW, CN)
    next SW  '\n' = (SW, CNL)
    next SW  '"'  = (SQ, CN)
    next SW  ','  = (SW, CCO)
    next SW  c    = (SX, CX c)
    next SX '\n'  = (SW, CNL)
    next SX '"'   = error "illegal double quote"
    next SX ','   = (SW, CCO)
    next SX c     = (SX, CX c)
    next SQ  '"'  = (SDQ, CN)
    next SQ  c    = (SQ, CX c)
    next SDQ '\n' = (SW, CNL)
    next SDQ '"'  = (SQ, CX '"')
    next SDQ ' '  = (SW, CN)
    next SDQ '\t' = (SW, CN)
    next SDQ ','  = (SW, CCO)
    next SDQ _    = error "illegal internal double quote"

collect :: [C] -> [[String]]
collect =
  foldr next []
  where
    next :: C -> [[String]] -> [[String]]
    next (CX x) [] = [[[x]]]
    next (CX x) ([]:rs) = [[x]]:rs
    next (CX x) ((w:ws):rs) = ((x:w):ws):rs
    next CCO [] = [[""]]  -- XXX shouldn't ever happen
    next CCO (r:rs) = ("":r):rs
    next CNL rs = [""]:rs
    next CN rs = rs

readCSV :: String -> [[String]]
readCSV = collect . label

primShowCSV :: (a -> String) -> [[a]] -> String
primShowCSV shower = 
  concatMap showRow
  where
    showRow = 
      (++ "\r\n") . intercalate "," . map (showField . shower)
      where
        showField s
          | null (s `intersect` " \t,\"\r\n") = s
          | otherwise = "\"" ++ foldr doublequote "" s ++ "\""
                        where
                          doublequote '\"' s' = '\"' : '\"' : s'
                          doublequote c s' = c : s'

showCSV :: [[String]] -> String
showCSV = primShowCSV id
