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
                 toNL, SSVReadException(..), 
                 SSVFormat(..), SSVFormatQuote(..),
                 csvFormat, pwfFormat)
where

import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Data.Typeable
import System.IO

-- | Formatting information for quoted strings for a
-- particular SSV variant.
data SSVFormatQuote = SSVFormatQuote {
  ssvFormatQuoteEscape :: Maybe Char,
  ssvFormatQuoteLeft :: Char,
  ssvFormatQuoteRight :: Char
}

-- | Formatting information for a particular SSV variant.
data SSVFormat = SSVFormat {
  ssvFormatName :: String,
  ssvFormatTerminator :: Char, -- ^ End of row.
  ssvFormatSeparator :: Char, -- ^ Field separator.
  ssvFormatEscape :: Maybe Char, -- ^ Escape character outside of quotes.
  ssvFormatStripWhite :: Bool, -- ^ Strip "extraneous" spaces and tabs.
  ssvFormatQuote :: Maybe SSVFormatQuote } -- ^ Quote format.

csvFormat :: SSVFormat
csvFormat = SSVFormat {
  ssvFormatName = "CSV",
  ssvFormatTerminator = '\n',
  ssvFormatSeparator = ',',
  ssvFormatEscape = Nothing,
  ssvFormatStripWhite = True,
  ssvFormatQuote = Just $ SSVFormatQuote {
    ssvFormatQuoteEscape = Just '"',
    ssvFormatQuoteLeft = '"',
    ssvFormatQuoteRight = '"' } }

pwfFormat :: SSVFormat
pwfFormat = SSVFormat {
  ssvFormatName = "Colon-separated record",
  ssvFormatTerminator = '\n',
  ssvFormatSeparator = ':',
  ssvFormatEscape = Nothing,
  ssvFormatStripWhite = False,
  ssvFormatQuote = Nothing }

-- | Indicates line and column and gives an error message.
data SSVReadException = SSVReadException String (Int, Int) String
                        deriving Typeable

instance Show SSVReadException where
  show (SSVReadException fmt (line, col) msg) =
    fmt ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ 
      "read error: " ++ msg

instance Exception SSVReadException

throwRE :: SSVFormat -> (Int, Int) -> String -> a
throwRE fmt pos msg =
  throw $ SSVReadException (ssvFormatName fmt) pos msg

-- State of the labeler.
data S = SW | -- reading a whitespace char
         SX | -- reading a generic char
         SQ | -- reading a quoted char
         SE | -- reading an escaped char
         SZ   -- reading a quoted-escaped char

-- Class of token output from the labeler
data C = CX Char | -- character
         CFS |     -- field separator
         CRS |     -- record separator
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
label :: SSVFormat -> String -> [C]
label fmt csv =
  run next (sw, (1, 1)) csv
  where
    -- State for initialization and fallback from end of field.
    sw
      | ssvFormatStripWhite fmt = SW
      | otherwise = SX
    -- Essentially mapAccumL, but with the test at the end
    -- so that it is fully lazy.
    run :: (SP -> Char -> (SP, C)) -> SP -> [Char] -> [C]
    run _ (s', pos') [] =
      case s' of
        SQ -> throwRE fmt pos' "unclosed quote in SSV"
        _ -> []
    run f s (x : xs) =
      let (s', c) = f s x in
      c : run f s' xs
    -- A bunch of abbreviations for concision.
    rs = ssvFormatTerminator fmt
    fs = ssvFormatSeparator fmt
    efmt = ssvFormatEscape fmt
    e = isJust efmt
    ec = fromJust efmt
    qfmt = ssvFormatQuote fmt
    q = isJust qfmt
    lq = ssvFormatQuoteLeft $ fromJust qfmt
    rq = ssvFormatQuoteRight $ fromJust qfmt
    qesc = ssvFormatQuoteEscape $ fromJust qfmt
    qe = isJust qesc
    eq = fromJust qesc
    -- Increment the position in the input various ways.
    incc (line, col) = (line, col + 1)
    incl (line, _) = (line + 1, 1)
    inct (line, col) = (line, tcol) 
                       where tcol = col + 8 - ((col + 7) `mod` 8)
    -- The actual state machine for the labeler.
    next :: SP -> Char -> (SP, C)
    next (SW, pos) ' '     = ((SW, incc pos), CN)
    next (SW, pos) '\t'    = ((SW, inct pos), CN)
    next (SW, pos) c 
      | c == rs            = ((sw, incl pos), CRS)
      | c == fs            = ((sw, incc pos), CFS)
      | e && c == ec       = ((SE, incc pos), CN)
      | q && c == lq       = ((SQ, incc pos), CN)
      | otherwise          = ((SX, incc pos), CX c)
    next (SX, pos) '\t'    = ((SX, inct pos), CX '\t')
    next (SX, pos) c 
      | c == rs            = ((sw, incl pos), CRS)
      | c == fs            = ((sw, incc pos), CFS)
      | e && c == ec       = ((SE, incc pos), CN)
      | q && c == lq       = throwRE fmt pos "illegal quote"
      | otherwise          = ((SX, incc pos), CX c)
    next (SQ, pos) '\t'    = ((SQ, inct pos), CX '\t')
    next (SQ, pos) c 
      | c == rs            = ((sw, incl pos), CRS)
      | q && qe && c == eq = ((SZ, incc pos), CN)
      | q && c == rq       = ((sw, incc pos), CN)
      | otherwise          = ((SQ, incc pos), CX c)
    next (SE, pos) '\t'    = ((SX, inct pos), CX '\t')
    next (SE, pos) c 
      | c == rs            = ((SX, incl pos), CX c)
      | otherwise          = ((SX, incc pos), CX c)
    next (SZ, pos) '\t'    = ((SW, inct pos), CN)
    next (SZ, pos) ' '     = ((SW, incc pos), CN)
    next (SZ, pos)  c 
      | c == rs            = ((sw, incl pos), CRS)
      | c == fs            = ((sw, incc pos), CFS)
      | q && qe && c == eq = ((SQ, incc pos), CX c)
      | q && c == rq       = ((SQ, incc pos), CX c)
      | q && c == lq       = ((SQ, incc pos), CX c)
      | otherwise          = throwRE fmt pos "illegal escape"

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
    next CFS [] = [["",""]]   -- no newline at end of file
    next CFS (r:rs) = ("":r):rs
    next CRS rs = [""]:rs
    next CN rs = rs

-- | Convert a 'String' representing a CSV file into a
-- properly-parsed list of rows, each a list of 'String'
-- fields. Adheres to the spirit and (mostly) to the letter
-- of RFC 4180, which defines the `text/csv` MIME type.
-- .
-- Rows are assumed to end at an unquoted newline. This
-- reader treats CR as a printable character, which may not
-- be what you want per RFC 4180. You may want to use 'toNL'
-- to clean up line endings as desired.
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
readCSV = collect . label csvFormat . toNL

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
