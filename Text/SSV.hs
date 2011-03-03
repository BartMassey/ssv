-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE DeriveDataTypeable #-}

-- | This modules provides conversion routines to and from
-- various "something-separated value" (SSV) formats.  In
-- particular, it converts the infamous "comma separated
-- value" (CSV) format.
module Text.SSV (readCSV, showCSV, hPutCSV, writeCSVFile,
                 toNL, CSVReadException(..))
where

import Control.Exception
import Data.Char
import Data.List
import Data.Typeable
import System.IO

-- | Indicates line and column and gives an error message.
data CSVReadException = CSVReadException (Int, Int) String
                        deriving Typeable

instance Show CSVReadException where
  show (CSVReadException (line, col) msg) =
    show line ++ ":" ++ show col ++ ": " ++ "CSV read error: " ++ msg

instance Exception CSVReadException

throwCSVException :: (Int, Int) -> String -> a
throwCSVException pos msg =
  throw (CSVReadException pos msg)

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

type SP = (S, (Int, Int))

-- Convert CR / LF sequences on input to LF (NL). Also convert
-- other CRs to LF.
toNL :: String -> String
toNL =
  foldr clean1 []
  where
    clean1 :: Char -> String -> String
    clean1 '\r' cs@('\n' : _) = cs
    clean1 '\r' cs = '\n' : cs
    clean1 c cs = c : cs

-- Run a state machine over the input to classify
-- all the characters.
label :: String -> [C]
label csv =
  run next (SW, (1, 1)) csv
  where
    run :: (SP -> Char -> (SP, C)) -> SP -> [Char] -> [C]
    run _ (s', pos') [] =
      case s' of
        SQ -> throwCSVException pos' "unclosed quote in CSV"
        _ -> []
    run f s (x : xs) =
      let (s', c) = f s x in
      c : run f s' xs
    next :: SP -> Char -> (SP, C)
    next (SW, pos)  ' '  = ((SW, incc pos), CN)
    next (SW, pos)  '\t' = ((SW, inct pos), CN)
    next (SW, pos)  '\n' = ((SW, incl pos), CNL)
    next (SW, pos)  '"'  = ((SQ, incc pos), CN)
    next (SW, pos)  ','  = ((SW, incc pos), CCO)
    next (SW, pos)  c    = ((SX, incc pos), CX c)
    next (SX, pos)  '\n' = ((SW, incl pos), CNL)
    next (SX, pos)  '"'  = throwCSVException pos "illegal double quote"
    next (SX, pos)  ','  = ((SW, incc pos), CCO)
    next (SX, pos)  '\t' = ((SX, inct pos), CX '\t')
    next (SX, pos)  c    = ((SX, incc pos), CX c)
    next (SQ, pos)  '"'  = ((SDQ, incc pos), CN)
    next (SQ, pos)  '\t' = ((SQ, inct pos), CX '\t')
    next (SQ, pos)  c    = ((SQ, incc pos), CX c)
    next (SDQ, pos) '\n' = ((SW, incl pos), CNL)
    next (SDQ, pos) '"'  = ((SQ, incc pos), CX '"')
    next (SDQ, pos) ' '  = ((SW, incc pos), CN)
    next (SDQ, pos) '\t' = ((SW, inct pos), CN)
    next (SDQ, pos) ','  = ((SW, incc pos), CCO)
    next (SDQ, pos) _    = throwCSVException pos "illegal closing double quote"
    incc (line, col) = (line, col + 1)
    incl (line, _) = (line + 1, 1)
    inct (line, col) = (line, tcol) 
                       where tcol = col + 8 - ((col + 7) `mod` 8)

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

-- | Convert a 'String' representing a CSV file into a
-- properly-parsed list of rows, each a list of 'String'
-- fields. Adheres to the spirit and (mostly) to the letter
-- of RFC 4180, which defines the `text/csv` MIME type.
-- .
-- Rows are assumed to end at an unquoted line
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
-- corresponds to the typical intent of CSV authors. Whitespace
-- on both sides of a quoted field is discarded. If a
-- double-quoted fields contains two double-quotes in a row,
-- these are treated as an escaped encoding of a single
-- double-quote.
-- .
-- The final line of the input may end with a line terminator,
-- which will be ignored, or without one.
readCSV :: String -> [[String]]
readCSV = collect . label . toNL

primShowCSV :: [[String]] -> String
primShowCSV = 
  concatMap showRow
  where
    showRow = 
      (++ "\n") . intercalate "," . map showField
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
-- to a single 'String' CSV representation. Adheres to the
-- spirit and (mostly) to the letter of RFC 4180, which
-- defines the `text/csv` MIME type.
-- .
-- Newline will be used as the end-of-line character, and no
-- discardable whitespace will appear in fields. Fields that
-- need to be quoted because they contain a special
-- character or line terminator will be quoted; all other
-- fields will be left unquoted. The final row of CSV will
-- end with a newline.
showCSV :: [[String]] -> String
showCSV = primShowCSV

primPutCSV :: Handle -> [[String]] -> IO ()
primPutCSV h csv = do
  hSetEncoding h utf8
  let nlm = NewlineMode { inputNL = nativeNewline, outputNL = CRLF }
  hSetNewlineMode h nlm
  hPutStr h $ primShowCSV csv

-- | Put a CSV representation of the given input out on a
-- file handle. Per RFC 4180, use CRLF as the line
-- terminator character.  Otherwise, this function behaves
-- as writing the output of 'showCSV' to the 'Handle'; if
-- you want native line terminators, this latter method
-- works for that.
hPutCSV :: Handle -> [[String]] -> IO ()
hPutCSV h csv = primPutCSV h csv

-- | Write a CSV representation of the given input
-- into a new file located at the given path. As with
-- 'hPutCSV', CRLF will be used as the line terminator.
writeCSVFile :: String -> [[String]] -> IO ()
writeCSVFile path csv = do
  h <- openFile path WriteMode
  hPutCSV h csv
  hClose h
