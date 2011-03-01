-- Copyright © 2011 Bart Massey

import CSV

main :: IO ()
main = interact (showCSV . readCSV)
  
