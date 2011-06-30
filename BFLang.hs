module BFLang where

type BFProgram = [BFStmt]

data BFStmt = PointerInc
            | PointerDec
            | Inc
            | Dec
            | Loop BFProgram
            | Print
            | Input
            deriving Show
