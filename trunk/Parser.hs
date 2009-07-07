module Parser where

import MudTypes
import Mud
import Text.Parsec
import Control.Applicative hiding (many)
import Control.Monad.Trans
import Data.Accessor
import Text

type MudParser = ParsecT String () Mud

pPlayer :: (Player -> Bool) -> MudParser (Id Player)
pPlayer f = try p <?> "player name" where
  p = do
    name <- pAnyWord
    (pl, _) : _ <- lift $ select $ \p -> case p ^. pName of
      Nothing -> False
      Just name' -> name `equalsIgnoreCase` name' && f p
    return pl

pObject :: (Object -> Bool) -> MudParser (Id Object)
pObject f = do
  os <- lift (select f)
  let p = do
        noun <- pAnyWord
        case filter ((== noun) . oNoun_ . snd) os of
          []            -> fail $ "no such item: " ++ noun
          (oid, _) : _  -> return oid
  try p <?> "item"

pAnyWord :: MudParser String
pAnyWord = many (noneOf " ") <* spaces

pWord :: String -> MudParser ()
pWord word = () <$ string word <* spaces

pRest :: MudParser String
pRest = many1 anyChar <* eof <?> "argument"

parse :: MudParser (Id Player -> Mud ()) -> Verb
parse grammar input player = do
  result <- runParserT grammar () "" input
  case result of
    Left e  -> tellLn player (show e)
    Right f -> f player
