module With (with) where

import Data.Accessor
-- import Data.Accessor.Tuple
import Control.Monad.State

-- | 'withBy' specialized for 'State'.
with :: Monad m => Accessor a b -> State b () -> StateT a m ()
with field action = field %: execState action

-- example :: State (Int, (Bool, Char)) ()
-- example = do
--   first =: 42
--   with second $ do
--     first  %: not
--     second =: 'q'
