-- | Some text utility functions.
module Text where

import Text.Regex.Posix
import Data.Char

nameOkay :: String -> Bool
nameOkay = (=~ "^[-'a-zA-Z]{3,16}$")

notEmpty :: String -> Bool
notEmpty = not . all isSpace

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

type MatchResult = (String, String, String, [String])

-- | Sanitizes and splits a command into verb and arguments.
--   For example: say hello there --> ("say", "hello there")
splitCommand :: String -> Maybe (String, String)
splitCommand s = case sanitizeInput s =~ "^([^a-zA-Z]+|[a-zA-Z]+)" :: MatchResult of
  (_, _, _, [])         -> Nothing
  (_, _, args, [verb])  -> Just (verb, args)
  _                     -> error "unexpected regex result"

sanitizeInput :: String -> String
sanitizeInput = filter charOk . trim
  where charOk c = ' ' <= c && c <= '~'

replaceAll :: String -> String -> String -> String
replaceAll pat sub input = case input =~ pat :: MatchResult of
  (before, _, "", _)    -> before
  (before, _, after, _) -> before ++ sub ++ replaceAll pat sub after

-- | listify ["a", "b", "c"] yields "a, b and c"
listify :: [String] -> String
listify [] = ""
listify [x] = x
listify [x, y] = x ++ " and " ++ y
listify (x:xs) = x ++ ", " ++ listify xs
