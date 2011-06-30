module BFMonad where

import Control.Monad.State
import Data.Char

import BFLang
import BFParser

newtype Memory a = Memory { unMemory :: StateT ([Int], [Int]) IO a }

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

    interpretStmt (Loop body) = do
      (front, e:end) <- get
      unless (e == 0) $ interpretProgram body
    
    interpretStmt Print = do
      (fs, e:es) <- get
      lift $ print $ chr e

-- test s = run p
--   where
--     Right p = parseTest program s
-- execState (interpretProgram [Inc]) ([], [0,0,0])
-- interpretStmt PointerInc = do
    --   (b, e, i) <- get
    --   put (b, e, i+1)
    --   return ()
      
    -- interpretStmt PointerDec = do
    --   (b, e, i) <- get
    --   put (b, e, i-1)
    --   return ()
      
    -- interpretStmt Inc = do
    --   (b, e, i) <- get
    --   let e' = (e !! i) + 1
    --   let e'' = (take (i-1) e) ++ [e'] ++ (drop (i+1) e)
    --   put (e'', i)
      
    -- interpretStmt Dec = do
    --   (e, i) <- get
    --   let e' = (e !! i) - 1
    --   let e'' = (take (i-1) e) ++ [e'] ++ (drop (i+1) e)
    --   put (e'', i)
      
      
-- run = interpretProgram $ execState (State {(listArray (0,1) [0,0]), 0})
