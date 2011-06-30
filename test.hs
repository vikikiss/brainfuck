import BFMonad
import BFParser

import Control.Monad.State
-- import Text.ParserCombinators.Parsec
-- test s = run p
--   where
--     Right p = parseTest program s
-- execStateT (interpretProgram [Inc]) ([], [0,0,0,0,0,0,0])


-- test s = run p
--   where
--     Right p = parseTest program s

run p = runState (interpretProgram p) ([], repeat 0)
