-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

{-# LANGUAGE DeriveDataTypeable #-}

-- | This modules provides conversion routines to and from
-- various \"something-separated value\" (SSV) formats.  In
-- particular, it converts the infamous \"comma separated
-- value\" (CSV) format.
module Text.SSV (
  -- * SSV format descriptions 
  -- | These records define a fairly flexible, if entirely
  -- kludgy, domain-specific language for describing
  -- \"something-separated value\" formats. An attempt is made
  -- in the reader and formatter to allow for fairly
  -- arbitrary combinations of features in a sane
  -- way. However, your mileage may undoubtedly vary; CSV is
  -- the only tested configuration.
  SSVFormat(..), 
  SSVFormatQuote(..),
  -- * SSV read, show and IO routines
  readSSV, 
  showSSV, 
  hPutSSV, 
  writeSSVFile, 
  -- * CSV read, show and IO routines 
  -- | CSV is a special case here. Partly this is by virtue
  -- of being the most common format.  CSV also needs a
  -- little bit of \"special\" help with input line endings
  -- to conform to RFC 4180.
  readCSV, 
  showCSV, 
  hPutCSV,
  writeCSVFile,
  -- * Newline conversions
  toNL,
  fromNL,
  -- * Exceptions
  SSVReadException(..), 
  SSVShowException(..),
  -- * Predefined formats
  csvFormat, 
  pwfFormat )
where

import Control.Exception
import Data.Char
import Data.List
import Data.Maybe
import Data.Set hiding (map)
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

-- | 'SSVFormat' for CSV data. Closely follows RFC 4180.
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

-- | 'SSVFormat' for UNIX \"password file\" data, i.e. colon-separated
-- fields with no escape convention.
pwfFormat :: SSVFormat
pwfFormat = SSVFormat {
  ssvFormatName = "Colon-separated values",
  ssvFormatTerminator = '\n',
  ssvFormatSeparator = ':',
  ssvFormatEscape = Nothing,
  ssvFormatStripWhite = False,
  ssvFormatQuote = Nothing }

-- | Indicates format name, line and column and gives an error message.
data SSVReadException = SSVReadException String (Int, Int) String
     		      | SSVEOFException String String
                        deriving Typeable

-- | Indicates format name and failed field and gives an
-- error message.  This should probably just be an 'error',
-- as the calling program is really responsible for passing
-- something formattable to the show routines.
data SSVShowException = SSVShowException String String String
                        deriving Typeable

instance Show SSVReadException where
  show (SSVReadException fmt (line, col) msg) =
    fmt ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ 
      "read error: " ++ msg
  show (SSVEOFException fmt msg) =
    fmt ++ ": read error at end of file: " ++ msg


instance Show SSVShowException where
  show (SSVShowException fmt s msg) =
    fmt ++ ": field " ++ show s ++ ": show error: " ++ msg

instance Exception SSVReadException

instance Exception SSVShowException

throwRE :: SSVFormat -> (Int, Int) -> String -> a
throwRE fmt pos msg =
  throw $ SSVReadException (ssvFormatName fmt) pos msg

throwSE :: SSVFormat -> String -> String -> a
throwSE fmt s msg =
  throw $ SSVShowException (ssvFormatName fmt) s msg

-- State of the labeler.
data S = SW | -- reading a whitespace char
         SX | -- reading a generic char
         SQ | -- reading a quoted char
         SE | -- reading an escaped char
         SZ | -- reading a quoted-escaped char
         SD   -- reading a post-quote char

-- Class of token output from the labeler
data C = CX Char | -- character
         CFS |     -- field separator
         CRS |     -- record separator
         CN        -- null token (discarded)

-- Line and column position in the input.
type P = (Int, Int)

-- | Convert CR / LF sequences on input to LF (NL). Also convert
-- other CRs to LF. This is probably the right way to handle CSV
-- data.
toNL :: String -> String
toNL =
  foldr clean1 []
  where
    clean1 :: Char -> String -> String
    clean1 '\r' cs@('\n' : _) = cs
    clean1 '\r' cs = '\n' : cs
    clean1 c cs = c : cs

-- | Convert LF (NL) sequences on input to CR LF. Leaves
-- | other CRs alone.
fromNL :: String -> String
fromNL =
  foldr dirty1 []
  where
    dirty1 :: Char -> String -> String
    dirty1 '\n' cs = '\r' : '\n' : cs
    dirty1 c cs = c : cs

-- Run a state machine over the input to classify
-- all the characters.
label :: SSVFormat -> String -> [C]
label fmt =
  run next incp (1, 1) sw
  where
    -- State for initialization and fallback from end of field.
    sw
      | ssvFormatStripWhite fmt = SW
      | otherwise = SX
    -- Essentially mapAccumL, but with the test at the end
    -- so that it is fully lazy, and with a "monadic" position.
    run :: (P -> S -> Char -> (S, C)) -> (P -> Char -> P) ->
           P -> S -> [Char] -> [C]
    run _ _ _ s' [] =
      case s' of
        SQ -> throw $ SSVEOFException (ssvFormatName fmt) "unclosed quote"
        _ -> []
    run f pf p s (x : xs) =
      let (s', c) = f p s x 
          p' = pf p x in
      c : run f pf p' s' xs
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
    incp (line, _) '\n' = (line + 1, 1)
    incp (line, col) '\t' = (line, tcol) 
      where tcol = col + 8 - ((col + 7) `mod` 8)
    incp (line, col) _ = (line, col + 1)
    -- The actual state machine for the labeler.
    next :: P -> S -> Char -> (S, C)
    next _ SW ' '     = (SW, CN)
    next _ SW '\t'    = (SW, CN)
    next _ SW c 
      | c == rs            = (sw, CRS)
      | c == fs            = (sw, CFS)
      | e && c == ec       = (SE, CN)
      | q && c == lq       = (SQ, CN)
      | otherwise          = (SX, CX c)
    next _ SX '\t'    = (SX, CX '\t')
    next pos SX c 
      | c == rs            = (sw, CRS)
      | c == fs            = (sw, CFS)
      | e && c == ec       = (SE, CN)
      | q && c == lq       = throwRE fmt pos "illegal quote"
      | otherwise          = (SX, CX c)
    next _ SQ '\t'    = (SQ, CX '\t')
    next _ SQ c 
      | c == rs            = (SQ, CX c)
      | q && qe && c == eq = (SZ, CN)
      | q && c == rq       = (SD, CN)
      | otherwise          = (SQ, CX c)
    next _ SE '\t'    = (SX, CX '\t')
    next _ SE c 
      | c == rs            = (SX, CX c)
      | otherwise          = (SX, CX c)
    next _ SZ '\t'    = (SD, CN)
    next _ SZ ' '     = (SD, CN)
    next pos SZ  c 
      | c == rs            = (sw, CRS)
      | c == fs            = (sw, CFS)
      | q && qe && c == eq = (SQ, CX c)
      | q && c == rq       = (SQ, CX c)
      | q && c == lq       = (SQ, CX c)
      | otherwise          = throwRE fmt pos "illegal escape"
    next _ SD ' '     = (SD, CN)
    next _ SD '\t'    = (SD, CN)
    next pos SD c 
      | c == rs            = (sw, CRS)
      | c == fs            = (sw, CFS)
      | otherwise          = throwRE fmt pos "junk after quoted field"

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

-- | Read using an arbitrary 'SSVFormat'. The input is not
-- cleaned with 'toNL'; if you want this, do it yourself.
-- The standard SSV formats 'csvFormat' and 'pwfFormat' are
-- provided.
readSSV :: SSVFormat -> String -> [[String]]
readSSV fmt = collect . label fmt

-- | Convert a 'String' representing a CSV file into a
-- properly-parsed list of rows, each a list of 'String'
-- fields. Adheres to the spirit and (mostly) to the letter
-- of RFC 4180, which defines the `text/csv` MIME type.
-- 
-- 'toNL' is used on the input string to clean up the
-- various line endings that might appear. Note that this
-- may result in irreversible, undesired manglings of CRs
-- and LFs.
-- 
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
-- 
-- The final line of the input may end with a line terminator,
-- which will be ignored, or without one.
readCSV :: String -> [[String]]
readCSV = readSSV csvFormat . toNL

-- | Show using an arbitrary 'SSVFormat'.  The standard SSV
-- formats 'csvFormat' and 'pwfFormat' are provided. Some
-- effort is made to \"intelligently\" quote the fields; in
-- the worst case an 'SSVShowException' will be thrown to
-- indicate that a field had characters that could not be
-- quoted.
showSSV :: SSVFormat -> [[String]] -> String
showSSV fmt = 
  concatMap showRow
  where
    showRow = 
      (++ "\n") . intercalate "," . map showField
      where
        -- Set of characters that require a field to be quoted.
        -- XXX This maybe could be kludgier, but I don't know how.
        scaryChars = fromList $ concat $ catMaybes [
           Just [ssvFormatTerminator fmt],
           Just [ssvFormatSeparator fmt],
           fmap (:[]) $ ssvFormatEscape fmt,
           fmap ((:[]) . ssvFormatQuoteLeft) $ ssvFormatQuote fmt, 
           case ssvFormatStripWhite fmt of
             True -> Just " \t"
             False -> Nothing ]
        -- Quote the field as needed.
        showField s
          | any notOkChar s = 
            case ssvFormatQuote fmt of
              Just qfmt ->
                if isJust (ssvFormatQuoteEscape qfmt) ||
                   not (elem (ssvFormatQuoteRight qfmt) s)
                then quote qfmt s
                else case ssvFormatEscape fmt of
                     Just ch -> escape ch s
                     Nothing -> throwSE fmt s "unquotable character in field"
              Nothing -> 
                case ssvFormatEscape fmt of
                  Just ch -> escape ch s
                  Nothing -> throwSE fmt s "unquotable character in field"
          | otherwise = s
            where
              notOkChar c | member c scaryChars = True
              notOkChar c | isSeparator c = ssvFormatStripWhite fmt
              notOkChar c | isPrint c = False
              notOkChar _ = True
              quote qfmt s' = [ssvFormatQuoteLeft qfmt] ++
                              qescape qfmt s' ++
                              [ssvFormatQuoteRight qfmt]
              escape esc s' =
                foldr escape1 "" s'
                where
                  escape1 c cs
                    | notOkChar c = esc : c : cs
                    | otherwise = c : cs
              qescape qfmt s' =
                case ssvFormatQuoteEscape qfmt of
                  Just qesc -> foldr (qescape1 qesc) "" s'
                  Nothing -> s'
                  where
                    qescape1 qesc c cs
                      | c == qesc || c == ssvFormatQuoteRight qfmt =
                        qesc : c : cs
                      | otherwise =
                        c : cs

-- | Convert a list of rows, each a list of 'String' fields,
-- to a single 'String' CSV representation. Adheres to the
-- spirit and (mostly) to the letter of RFC 4180, which
-- defines the `text/csv` MIME type.
-- 
-- Newline will be used as the end-of-line character, and no
-- discardable whitespace will appear in fields. Fields that
-- need to be quoted because they contain a special
-- character or line terminator will be quoted; all other
-- fields will be left unquoted. The final row of CSV will
-- end with a newline.
showCSV :: [[String]] -> String
showCSV = showSSV csvFormat

-- | Put a representation of the given SSV input out on a
-- file handle using the given 'SSVFormat'. Uses CRLF as the
-- line terminator character, as recommended by RFC 4180 for
-- CSV.  Otherwise, this function behaves as writing the
-- output of 'showSSV' to the 'Handle'; if you want native
-- line terminators, this latter method works for that.
hPutSSV :: SSVFormat -> Handle -> [[String]] -> IO ()
hPutSSV fmt h csv = do
  hSetEncoding h utf8
  let nlm = NewlineMode { inputNL = nativeNewline, outputNL = CRLF }
  hSetNewlineMode h nlm
  hPutStr h $ showSSV fmt csv

-- | Perform 'hPutSSV' with 'csvFormat'.
hPutCSV :: Handle -> [[String]] -> IO ()
hPutCSV = hPutSSV csvFormat

-- | Write an SSV representation of the given input into a
-- new file located at the given path, using the given
-- 'SSVFormat'. As with 'hPutCSV', CRLF will be used as the
-- line terminator.
writeSSVFile :: SSVFormat -> String -> [[String]] -> IO ()
writeSSVFile fmt path csv = do
  h <- openFile path WriteMode
  hPutSSV fmt h csv
  hClose h

-- | Perform 'writeSSVFile' with 'csvFormat'.
writeCSVFile :: String -> [[String]] -> IO ()
writeCSVFile = writeSSVFile csvFormat
