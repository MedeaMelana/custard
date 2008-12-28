module With (withBy, with) where

import Data.Accessor
import Data.Accessor.Tuple
import Control.Monad.State

-- | Causes all state actions in the argument to be computed relative to the accessor.
withBy :: MonadState a m => (n b c -> b -> b) -> Accessor a b -> n b c -> m ()
withBy run field action = field %: run action

-- | 'withBy' specialized for 'State'.
with :: MonadState a m => Accessor a b -> State b () -> m ()
with = withBy execState

example :: State (Int, (Bool, Char)) ()
example = do
  first =: 42
  with second $ do
    first  %: not
    second =: 'q'
