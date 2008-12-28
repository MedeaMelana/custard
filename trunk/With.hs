module With where

import Data.Accessor
import Data.Accessor.Tuple
import Control.Monad.State

withBy :: MonadState a m => (n b c -> b -> b) -> Accessor a b -> n b c -> m ()
withBy run field action = field %: run action

with :: MonadState a m => Accessor a b -> State b () -> m ()
with = withBy execState

example :: State (Int, (Bool, Char)) ()
example = do
  first =: 42
  with second $ do
    first  %: not
    second =: 'q'
