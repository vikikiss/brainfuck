import BFMonad
import BFParser

import qualified Control.Monad.State as S
import Text.ParserCombinators.Parsec
-- test s = run p
--   where
--     Right p = parseTest program s
-- execStateT (interpretProgram [Inc]) ([], [0,0,0,0,0,0,0])


-- test s = run p
--   where
--     Right p = parseTest program s



runFile f = do
  prog <- parseFromFile program f
  case prog of
    Left err -> error $ show err
    Right prog -> run prog

run p = S.runStateT (interpretProgram p) ([], replicate 10 0)
