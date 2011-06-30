{- LANGUAGE NoMonomorphismRestricion -}
module BFParser where

import Text.ParserCombinators.Parsec 

import BFLang

program :: Parser BFProgram
program = do
  skipMany space
  stmts <- many stmt
  eof
  return stmts
  
stmt :: Parser BFStmt
stmt = choice [pointerInc, pointerDec, inc, dec, loop, print, input]
  where
    pointerInc = do
      keyword '>'
      return PointerInc
      
    pointerDec = do
      keyword '<'
      return PointerDec
    
    inc = do
      keyword '+'
      return Inc
      
    dec = do
      keyword '-'
      return Dec
      
    loop = do
      body <- between (char '[') (char ']') (many stmt)
      return $ Loop body
      
    print = do
      keyword '.'
      return Print
      
    input = do
      keyword ','
      return Input
      
keyword :: Char -> Parser ()
keyword c = do
  char c
  skipMany space
          
-- test = do Right [filename] <- parseFromFile program "hello"
test = parseTest program ">"
