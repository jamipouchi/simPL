{-# LANGUAGE BlockArguments #-}

module Main where

import Interpreter
import Lexer
import LookUpTable
import Parser

-- | Entry point. You execute with a file, and it executes the instructions
main :: IO LookUpTable
main = do
  putStrLn "Do you want to load a file [y], or use as interpreter [n]"
  option <- getChar
  case option of
    'y' -> executeFromFile
    'n' -> executeFromTerminal
    unrecognized -> do
      putStrLn $ "unrecognized option: " ++ [unrecognized]
      main

executeFromFile :: IO LookUpTable
executeFromFile = do
  content <- readFileToString
  let tokens = getTokens content
  let instructions = tokensToInstr tokens
  lut <- exec (Seq instructions) empty
  print lut
  main

executeFromTerminal :: IO LookUpTable
executeFromTerminal = do
  _ <- getLine
  content <- getLine
  let tokens = getTokens content
  let instructions = tokensToInstr tokens
  lut <- exec (Seq instructions) empty
  print lut
  main

-- -- Space is 32, } is 125. So we accept chars >= 32 and <= 125
readFileToString :: IO [Char]
readFileToString = do
  path <- getContents
  input <- readFile path
  let content = filter (\c -> fromEnum c >= 32 && fromEnum c <= 125) input
  return content