module Koan.Parser.Json where

import Control.Applicative
import Control.Monad          (void)
import Data.Char
import Text.Megaparsec        hiding (crlf)
import Text.Megaparsec.Expr
import Text.Megaparsec.String hiding (crlf)

import qualified Text.Megaparsec.Lexer as L

enrolled :: Bool
enrolled = True

data Json
  = JsonBool    Bool
  | JsonNumber  Double
  | JsonString  String
  | JsonArray   [Json]
  | JsonObject  [(String, Json)]
  | JsonNull
  deriving (Eq, Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

escapees :: String
escapees = "\\\"\n\r\t"

escapeeChars :: String
escapeeChars = "\\\"nrt"

-- Hint: Use between function
brackets :: Parser a -> Parser a
brackets = error "TODO Implement brackets"

braces :: Parser a -> Parser a
braces = error "TODO Implement braces"

plainChar :: Parser Char
plainChar = undefined

escapedChar :: Parser Char
escapedChar = char '\\' *> plainChar

litString :: Parser String
litString = char '"' *> many plainChar <* char '"'

litBool :: Parser Bool
litBool = do
  bool <- litString
  case bool of
    "true" -> return True
    _      -> return False

comma :: Parser ()
comma = char ',' *> return ()

array :: Parser [Json]
array = char '[' *> json `sepBy` char ',' <* char ']'

field :: Parser (String, Json)
-- field = (,) <$> (litString <* char ':') <*> json
field = (\a b -> (a, b)) <$> (litString <* char ':') <*> json

object :: Parser [(String, Json)]
object = char '{' *>
  field  `sepBy` char ','
  <* char '}'

-- object = error "TODO Implement json"

nullKeyword :: Parser ()
nullKeyword = return ()

json :: Parser Json
json = const JsonNull <$> nullKeyword
