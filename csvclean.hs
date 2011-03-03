-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

import Text.SSV

main :: IO ()
main = interact (showCSV . readCSV)
  
