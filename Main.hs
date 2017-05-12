{-# LANGUAGE QuasiQuotes #-}

import Afx


add_And_ :: String -> String -> String
add_And_ = (++)

_IsBetween_And_ :: Ord a => a -> a -> a -> Bool
_IsBetween_And_ a b c = a > b && a < c


main = let a = "a"
           b = "b"
           c = 3
           d = 2
           e = 5
        in do 
          print ([afx| add °a and °b |])
          print ([afx| °c is between °d and °e |])
          print ([afx| #3 is between #2 and #5 |])
          print ([afx| #2.1 is between #2.0 and #3.1 |])
