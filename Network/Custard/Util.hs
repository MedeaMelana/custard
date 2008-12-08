module Network.Custard.Util where

import qualified Data.Char as C
import Control.Concurrent

notEmpty :: String -> Bool
notEmpty = not . all C.isSpace

trim :: String -> String
trim = f . f where f = reverse . dropWhile C.isSpace

mapMVar :: MVar a -> (a -> a) -> IO (a, a)
mapMVar m f = modifyMVar m t where
  t v = return (v', (v, v')) where 
    v' = f v

listify :: [String] -> String
liftify [] = ""
listify [x] = x
listify [x, y] = x ++ " and " ++ y
listify (x:xs) = x ++ ", " ++ listify xs
