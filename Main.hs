{-# LANGUAGE QuasiQuotes #-}

import Afx


add_And_ :: Num a => a -> a -> a
add_And_ = (+)

_IsBetween_And_ :: Ord a => a -> a -> a -> Bool
_IsBetween_And_ a b c = [afx| (°a greater than °b) and (°a lower than °c) |]

_LowerThan_, _GreaterThan_ :: Ord a => a -> a -> Bool
_LowerThan_ = (<)
_GreaterThan_ = (>)

_And_ :: Bool -> Bool -> Bool
_And_ = (&&) 

main = do 
  let a = 1
      b = 2
      c = 3
  print [afx| °a is between °b and °c |]
  print [afx| °b is between °a and °c |]
    
