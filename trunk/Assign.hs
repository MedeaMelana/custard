{-# OPTIONS -fglasgow-exts #-}

module Assign where

import qualified Control.Monad.State as S
import Data.Accessor
import Data.Accessor.Tuple

data Assign s a where
  State :: S.State s a              -> Assign s a
  (:=) :: Accessor s t  -> t        -> Assign s ()
  (:~) :: Accessor s t  -> (t -> t) -> Assign s ()

instance Monad (Assign s) where
  return v  = State $ return v
  a >>= f   = State $ asState a >>= asState . f

instance S.MonadState s (Assign s) where
  get = State S.get
  put = State . S.put

asState :: Assign s a -> S.State s a
asState (State s) = s
asState (field := value) = putA field value
asState (field :~ image) = modA field image

withBy :: S.MonadState a m => (n b c -> b -> b) -> Accessor a b -> n b c -> m ()
withBy run field action = field %: run action

with :: S.MonadState a m => Accessor a b -> Assign b () -> m ()
with = withBy (S.execState . asState)

example :: Assign (Int, (Bool, Char)) ()
example = do
  first := 42
  with second $ do
    first  :~ not
    second := 'q'
