{-# LANGUAGE TemplateHaskell #-}

module Afx (afx) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List
import Data.Char


afx = QuasiQuoter 
  { quoteExp = matchCallToFunction
  , quoteDec = undefined
  , quotePat = undefined
  , quoteType = undefined
  }

matchCallToFunction :: String -> ExpQ
matchCallToFunction [] = error "empty expression"
matchCallToFunction xs 
  = let xs' = words xs
        v = args xs'
        f = functionName xs'
    in go (reverse v) f
  where
    go :: [String] -> String -> ExpQ
    go []     ks = undefined
    go [('#':x)]    ks = appE (dyn ks) [|x|] 
    go [('°':x)]    ks = appE (dyn ks) (dyn x)
    go (('#':x):vs) ks = appE (go vs ks) [|x|]
    go (('°':x):vs) ks = appE (go vs ks) (dyn x)

isVar, isLit, isVarOrLit :: String -> Bool 
isVar x = head x == '°'
isLit x = head x == '#'
isVarOrLit x = isVar x || isLit x

args, keywords :: [String] -> [String]
args = filter (isVarOrLit)
keywords = filter (not . isVarOrLit)

camelCase :: String -> String
camelCase [] = []
camelCase (x:xs) = toUpper x : map toLower xs

haskellizeCC :: String -> String
haskellizeCC [] = []
haskellizeCC (x:xs) = toLower x : xs

transformWord :: String -> String
transformWord x
  | isVarOrLit x = "_"
  | otherwise = camelCase x

functionName :: [String] -> String
functionName [] = error "empty function name call"
functionName xs =  haskellizeCC (concat (map transformWord xs)) 
