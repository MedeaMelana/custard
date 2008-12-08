{-# OPTIONS -fglasgow-exts #-}

module Network.Custard where

import Network
import System.IO
import Control.Concurrent
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C
import qualified Data.Map as M
import Data.Maybe

