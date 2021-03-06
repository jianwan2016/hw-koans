module Koan.Parser.Csv where

import Control.Applicative
import Control.Monad (void)
import Data.Char
import Text.Megaparsec hiding (crlf)
import Text.Megaparsec.Expr
import Text.Megaparsec.String hiding (crlf)

import qualified Text.Megaparsec.Lexer as L

enrolled :: Bool
enrolled = False

-- ABNF grammar:
--
--  file        = record *(LF record) [LF]
--  record      = field *(COMMA field)
--  field       = (escaped / non-escaped)
--  escaped     = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
--  non-escaped = *TEXTDATA
--  COMMA       = %x2C
--  CR          = %x0D
--  DQUOTE      = %x22
--  LF          = %x0A
--  TEXTDATA    = %x20-21 / %x23-2B / %x2D-7E

parseCsv :: String -> Either (ParseError (Token String) Dec) [[String]]
parseCsv = parse file ""

file :: Parser [[String]]
file = emptyFile <|> populatedFile
  where populatedFile = (:) <$> record <*> many (try (lf *> notFollowedBy eof *> record)) <* optional lf <* eof
        emptyFile     = eof *> pure []

record :: Parser [String]
record = field `sepBy` comma

field :: Parser String
field = escaped <|> nonEscaped

escaped :: Parser String
escaped = dquote *> quoted <* dquote
  where quoted  = many (try (textdata <|> comma <|> cr <|> lf <|> dq))
        dq      = dquote >> dquote

nonEscaped :: Parser String
nonEscaped = many textdata

comma :: Parser Char
comma = char ','

cr :: Parser Char
cr = char '\r'

dquote :: Parser Char
dquote = char '"'

lf :: Parser Char
lf = char '\n'

textdata :: Parser Char
textdata = satisfy $ \c ->
  c == chr 0x20 ||
  c == chr 0x21 ||
  (c >= chr 0x23 && c <= chr 0x2B) ||
  (c >= chr 0x2D && c <= chr 0x7E)
