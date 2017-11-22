{-# LANGUAGE QuasiQuotes #-}

import Afx


add_And :: Num a => a -> a -> a
add_And = (+)

_IsBetween_And :: Ord a => a -> a -> a -> Bool
_IsBetween_And a b c = [afx| (°a greater than °b) and (°a lower than °c) |]

_LowerThan, _GreaterThan :: Ord a => a -> a -> Bool
_LowerThan = (<)
_GreaterThan = (>)

_And :: Bool -> Bool -> Bool
_And = (&&) 

main = do 
  let a = 1
      b = 2
      c = 3
  print [afx| °a is between °b and °c |]
  print [afx| °b is between °a and °c |]
  print [afx| 2 is between 1 and 3 |]
  -- note that "id" is the normal implementation from prelude
  print [afx| 2 is between (id °a) and (add 1 and °b) |]
  [afx| putStrLn "asdf" |]
  [afx| print True |]
 
