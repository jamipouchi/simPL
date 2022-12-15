{-# LANGUAGE BlockArguments #-}

module Main where

import Lexer
import LookUpTable
import Parser
import SimpleLP

main :: IO LookUpTable
main = do
  content <- fileToString
  let tokens = getTokens content
  let instructions = tokensToInstr tokens
  exec (Seq instructions) empty

mainFromString :: String -> IO LookUpTable
mainFromString str = do
    let tokens = getTokens str
    let instructions = tokensToInstr tokens
    exec (Seq instructions) empty

-- -- Space is 32, } is 125. So we accept chars >= 32 and <= 125
-- This is the function to use. Prob will be moved to main
fileToString :: IO [Char]
fileToString = do
  input <- readFile "test.txt"
  let content = filter (\c -> fromEnum c >= 32 && fromEnum c <= 125) input
  return content