module BFMonad where

import Control.Monad.State
import Data.Char
import Data.Word

import BFLang
import BFParser

-- type Memory a = Memory { unMemory :: StateT ([Word8], [Word8]) IO a }
type Memory a = StateT ([Word8], [Word8]) IO a

interpretProgram :: BFProgram -> Memory ()
interpretProgram pr = mapM_ interpretStmt pr
  where
    -- ([], [1,2,3,4])
    -- ([1], [2,3,4])
    -- ([2,1], [3, 4])
    interpretStmt PointerInc = do
      (fs, e:es) <- get
      put (e:fs, es)
    
    interpretStmt PointerDec = do
      (f:fs, es) <- get
      put (fs, f:es)
      
    interpretStmt Inc = do
      (front, end) <- get
      put (front, [(head end) + 1] ++ (drop 1 end))

    interpretStmt Dec = do
      (front, end) <- get
      put (front, [(head end) - 1] ++ (drop 1 end))

    interpretStmt loop@(Loop body) = do
      (front, e:end) <- get
      unless (e == 0) $ do interpretProgram body
                           interpretStmt loop
    
    interpretStmt Print = do
      (fs, e:es) <- get
      lift $ putChar $ chr $ fromIntegral e
