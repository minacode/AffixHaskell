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
  --quoteExp = matchCallToFunction
  , quoteDec = undefined
  , quotePat = undefined
  , quoteType = undefined
  }

camelCase :: String -> String
camelCase [] = []
camelCase (x:xs) = toUpper x : map toLower xs

haskellizeCC :: String -> String
haskellizeCC [] = []
haskellizeCC (x:xs) = toLower x : xs

strip :: String -> String
strip [] = []
strip s@(x:xs)
  | isSpace x = strip xs
  | otherwise = s

parse :: String -> Expression
parse [] = error "Afx Parsing: Empty String"
parse xs = Branch (parse' $ strip xs)

parse' :: String -> [Expression]
parse' [] = []
parse' ('(':xs) =
  let (e, rest) = parseParens xs
   in (Branch (parse' e)) : parse' (strip rest)
parse' ('#':xs) =
  let (e, rest) = span (not . isSpace) xs
   in Leaf (Lit e) : parse' (strip rest)
parse' ('°':xs) =
  let (e, rest) = span (not . isSpace) xs
   in Leaf (Var e) : parse' (strip rest)
parse' s@(x:xs) =
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
functionName at = haskellizeCC . concat $ map (camelCase . getRepr) at
    
getRepr :: Expression -> String
getRepr (Leaf (Keyword name)) = name
getRepr _ = "_"

getNonKeywordNodes :: [Expression] -> [Expression]
getNonKeywordNodes ((Leaf (Keyword _)) : xs) = getNonKeywordNodes xs
getNonKeywordNodes (x:xs) = x : getNonKeywordNodes xs
getNonKeywordNodes [] = []

generateQExpr :: Expression -> ExpQ
generateQExpr (Leaf (Lit s)) = [|s|]
generateQExpr (Leaf (Var s)) = (dyn s)
generateQExpr (Leaf (Keyword s)) = (dyn s)
generateQExpr (Branch es) =
  let fName = functionName es
      nonKwNodes   = reverse $ getNonKeywordNodes es
   in go fName nonKwNodes

go :: String -> [Expression] -> ExpQ 
go f [] = (dyn f)
go f (nkn:nkns) = appE (go f nkns) (generateQExpr nkn)
