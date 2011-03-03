-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- | This modules provides conversion routines to and
-- from the infamous "comma separated value" (CSV) format.
-- It attempts to adhere to the spirit and (mostly) to the
-- letter of RFC 4180, which defines the `text/csv` MIME
-- type.
module Text.CSV (readCSV, readCSV', showCSV, showCSV', 
                 hPutCSV, hPutCSV', writeCSVFile, writeCSVFile')
where

import Data.Char
import Data.List
import System.IO

-- State of the labeler.
data S = SW | -- reading whitespace
         SX | -- reading other chars
         SQ | -- reading quoted chars
         SDQ  -- just saw a double quote

-- Class of token output from the labeler
data C = CX Char | -- character
         CCO |     -- comma
         CNL |     -- newline
         CN        -- null token (discarded)

-- Run a state machine over the input to classify
-- all the characters.
label :: String -> [C]
label csv =
  let (s', cs) = mapAccumL next SW csv in
  case s' of
    SQ -> error "unclosed quote in CSV"
    _ -> cs
  where
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

-- Convert CR / LF sequences on input to NL. Also convert
-- other CRs to newlines. On input, LFs are already NLs.
cleancr :: [C] -> [C]
cleancr =
  foldr clean1 []
  where
    clean1 :: C -> [C] -> [C]
    clean1 (CX '\r') cs@(CNL : _) = cs
    clean1 (CX '\r') cs = CNL : cs
    clean1 c cs = c : cs

-- Convert the class tokens into a list of rows, each
-- consisting of a list of strings.
collect :: [C] -> [[String]]
collect =
  foldr next []
  where
    next :: C -> [[String]] -> [[String]]
    next (CX x) [] = [[[x]]]
    next (CX x) ([]:rs) = [[x]]:rs
    next (CX x) ((w:ws):rs) = ((x:w):ws):rs
    next CCO [] = [["",""]]   -- no newline at end of file
    next CCO (r:rs) = ("":r):rs
    next CNL rs = [""]:rs
    next CN rs = rs

-- | Convert a 'String' representing a CSV file into
-- a properly-parsed list of rows, each a list of 'String'
-- fields. 
-- .
-- This conversion follows RFC 4180 to a reasonable
-- degree. Rows are assumed to end at an unquoted line
-- terminator: CRLF, CR, or LF. Quoted line terminators on the
-- input will be converted to newlines in the resulting field.
-- (Note that this canonicalization loses the distinction between
-- the various quoted line terminators.
-- .
-- Fields are expected to be separated by commas. Per RFC
-- 4180, fields may be double-quoted: only whitespace, which
-- is discarded, may appear outside the double-quotes of a
-- quoted field. For unquoted fields, whitespace to the left
-- of the field is discarded, but whitespace to the right is
-- retained; this is convenient for the parser, and probably
-- corresponds to the typical intent of CSV authors. If a
-- double-quoted fields contains two double-quotes in a row,
-- these are treated as an escaped encoding of a single
-- double-quote.
-- .
-- The final line of the input may end with a line terminator,
-- which will be ignored, or without one.
readCSV :: String -> [[String]]
readCSV = collect . cleancr . label

-- | This convenience function converts a CSV 'String' to a
-- set of fields by calling 'readCSV' and applying 'read' to
-- each field.
readCSV' :: Read a => String -> [[a]]
readCSV' = map (map read) . readCSV

primShowCSV :: (a -> String) -> [[a]] -> String
primShowCSV shower = 
  concatMap showRow
  where
    showRow = 
      (++ "\n") . intercalate "," . map (showField . shower)
      where
        showField s
          | all okChar s = s
          | otherwise = "\"" ++ foldr doublequote "" s ++ "\""
            where
              okChar '"' = False
              okChar ',' = False
              okChar c | isSeparator c = False
              okChar c | isPrint c = True
              okChar _ = False
              doublequote '\"' s' = '\"' : '\"' : s'
              doublequote c s' = c : s'

-- | Convert a list of rows, each a list of 'String' fields,
-- to a single 'String' CSV representation. Newline will be
-- used as the end-of-line character, and no discardable
-- whitespace will appear in fields. Fields that need to be
-- quoted because they contain a special character or line
-- terminator will be quoted; all other fields will be left
-- unquoted. The final row of CSV will end with a newline.
showCSV :: [[String]] -> String
showCSV = primShowCSV id

-- | This convenience function will call 'showCSV', applying
-- 'show' at the appropriate point to convert each field to
-- a 'String'.
showCSV' :: Show a => [[a]] -> String
showCSV' = primShowCSV show

primPutCSV :: (a -> String) -> Handle -> [[a]] -> IO ()
primPutCSV shower h csv = do
  hSetEncoding h utf8
  let nlm = NewlineMode { inputNL = nativeNewline, outputNL = CRLF }
  hSetNewlineMode h nlm
  hPutStr h $ primShowCSV shower csv

-- | Put a CSV representation of the given input out on a
-- file handle. Per RFC 4180, use CRLF as the line
-- terminator character.  Otherwise, this function behaves
-- as writing the output of 'showCSV' to the 'Handle'; if
-- you want native line terminators, this latter method
-- works for that.
hPutCSV :: Handle -> [[String]] -> IO ()
hPutCSV h csv = primPutCSV id h csv

-- | This convenience function will call 'hPutCSV', applying
-- 'show' at the appropriate point to convert each field to
-- a 'String'.
hPutCSV' :: Show a => Handle -> [[a]] -> IO ()
hPutCSV' h csv = primPutCSV show h csv

-- | Write a CSV representation of the given input
-- into a new file located at the given path. As with
-- 'hPutCSV', CRLF will be used as the line terminator.
writeCSVFile :: String -> [[String]] -> IO ()
writeCSVFile path csv = do
  h <- openFile path WriteMode
  hPutCSV h csv

-- | This convenience function will call 'writeCSVFile', applying
-- 'show' at the appropriate point to convert each field to
-- a 'String'.
writeCSVFile' :: Show a => String -> [[a]] -> IO ()
writeCSVFile' path csv = do
  h <- openFile path WriteMode
  hPutCSV' h csv