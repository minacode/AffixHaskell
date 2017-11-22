{-# LANGUAGE TemplateHaskell #-}

-- TODO
-- parse correct types for Lits
-- parse parens correct.. next ) is obviously wrong

module Afx (afx, parse, generateQExpr) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List
import Data.Char
import Control.Monad.State.Strict

data Tree a = Branch [Tree a] | Leaf a deriving Show
type Expression = Tree Atom 
data Atom = Lit String | Var String | Keyword String deriving Show
     
 
afx = QuasiQuoter 
  { quoteExp = generateQExpr . parse
  , quoteDec = undefined
  , quotePat = undefined
  , quoteType = undefined
  }

camelCase :: String -> String
camelCase [] = []
camelCase (x:xs) = toUpper x : xs -- map toLower xs

haskellizeCC :: String -> String
haskellizeCC [] = []
haskellizeCC (x:xs) = toLower x : xs

strip :: String -> String
strip [] = []
strip s@(x:xs)
  | isSpace x = strip xs
  | otherwise = s

stripEnd :: String -> String
stripEnd xs = reverse . (dropWhile (== '_')) $ reverse xs

parse :: String -> Expression
parse [] = error "Afx Parsing: Empty String"
parse xs = Branch (parse' $ strip xs)

parse' :: String -> [Expression]
parse' [] = []
parse' ('(':xs) =
  let (e, rest) = parseParens xs
   in (Branch (parse' e)) : parse' (strip rest)
parse' ('°':xs) =
  let (e, rest) = span (not . isSpace) xs
   in Leaf (Var e) : parse' (strip rest)
parse' s@(x:_) = 
  if isDigit x || x == '"' || x == '\'' || isUpper x then
    let (e, rest) = span (not . isSpace) s
     in Leaf (Lit e) : parse' (strip rest)
  else
    let (e, rest) = span (not . isSpace) s
     in Leaf (Keyword e) : parse' (strip rest)

parseParens :: String -> (String, String)
parseParens xs = 
  let (index, _) = execState (findParens xs) (0,1)
      (a, b) = splitAt index xs
   in (a, tail b)

findParens :: String -> State (Int, Int) ()
findParens [] = undefined
findParens ('(':xs) = do
  (parsed, pCount) <- get
  put (parsed +1, pCount +1)
  findParens xs
findParens (')':xs) = do
  (parsed, pCount) <- get
  if pCount /= 1
  then do 
    put (parsed +1, pCount -1)
    findParens xs
  else return ()
findParens (x:xs) = do
  (parsed, pCount) <- get
  put (parsed +1, pCount)
  findParens xs

functionName :: [Expression] -> String
functionName at = stripEnd . haskellizeCC . concat $ map (camelCase . getRepr) at
    
getRepr :: Expression -> String
getRepr (Leaf (Keyword name)) = name
getRepr _ = "_"

getNonKeywordNodes :: [Expression] -> [Expression]
getNonKeywordNodes ((Leaf (Keyword _)) : xs) = getNonKeywordNodes xs
getNonKeywordNodes (x:xs) = x : getNonKeywordNodes xs
getNonKeywordNodes [] = []

isInteger, isFloat :: String -> Bool
isInteger s = and (map isNumber s)
isFloat s = evalState (testFloat s) 0

testFloat :: String -> State Int Bool
testFloat [] = do
  count <- get
  return (count == 1)
testFloat ('.':xs) = do
  modify (+1)
  testFloat xs
testFloat (x:xs)
  | isNumber x = do
    testFloat xs
  | otherwise = return False 
   
-- TODO
generateQExpr :: Expression -> ExpQ
generateQExpr (Leaf (Lit s@(x:xs)))
  | isInteger s = litE (integerL (read s))
  -- | isFloat s = litE (rationalL (read s))
  | x == '\'' = litE (charL (read s))
  | otherwise = litE (stringL s)
generateQExpr (Leaf (Var s)) = (dyn s)
generateQExpr (Leaf (Keyword s)) = (dyn s)
generateQExpr (Branch es) =
  let fName = functionName es
      nonKwNodes   = getNonKeywordNodes es
   in appsE (dyn fName : (map generateQExpr nonKwNodes))
