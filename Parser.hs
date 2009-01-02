module Parser where

import MudTypes
import Mud
import Text.Parsec
import Control.Applicative hiding (many)
import Control.Monad.Trans

type MudParser = ParsecT String () Mud

pPlayer :: MudParser (Id Player)
pPlayer = try p <?> "player name" where
  p = do
    name <- pWord
    mp <- lift (playerByName name)
    case mp of
      Nothing -> empty
      Just p  -> return p

pWord :: MudParser String
pWord = many (noneOf " ") <* spaces

pRest :: MudParser String
pRest = many anyChar <* eof

parse :: MudParser (Id Player -> Mud ()) -> Verb
parse grammar input player = do
  result <- runParserT grammar () "" input
  case result of
    Left e  -> tellLn player (show e)
    Right f -> f player
