{-# LANGUAGE BlockArguments #-}

module Main where

import Interpreter
import Lexer
import LookUpTable
import Parser

-- | Entry point. You execute with a file, and it executes the instructions
main :: IO LookUpTable
main = do
  content <- readFileToString
  let tokens = getTokens content
  let instructions = tokensToInstr tokens
  exec (Seq instructions) empty
  main

mainFromString :: String -> IO LookUpTable
mainFromString str = do
  let tokens = getTokens str
  let instructions = tokensToInstr tokens
  exec (Seq instructions) empty

-- -- Space is 32, } is 125. So we accept chars >= 32 and <= 125
readFileToString :: IO [Char]
readFileToString = do
  path <- getContents
  input <- readFile path
  let content = filter (\c -> fromEnum c >= 32 && fromEnum c <= 125) input
  return content