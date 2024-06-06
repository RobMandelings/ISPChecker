{-# LANGUAGE OverloadedStrings #-}

module DSLParser where

-- Qualified vs Non-qualified: Qualified makes sure you need the prefix to access the exported functionality. This is not necessary with normal import.

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T -- Qualified

-- Parsec is the core parser type in Megaparsec. Represents a parser that can consume input and produce a result.
-- Void: error type (don't care about custom error information; TODO later)
-- Text: type of input the parser works on (string/bytestring/text)
-- TODO difference Text and String?
type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

{- | Returns a parser that consumes any sequence of characters followed by whitespace. -}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

parseName :: Parser String
parseName = lexeme $ string "name:" >> stringLiteral

parseDescription :: Parser String
parseDescription = lexeme $ string "description:" >> stringLiteral

parseCourses :: Parser [String]
parseCourses = lexeme $ string "courses:" >> between (symbol "[") (symbol "]") (identifier `sepBy` symbol ",")

parseConstraints :: Parser [String]
parseConstraints = lexeme $ string "constraints:" >> between (symbol "[") (symbol "]") (identifier `sepBy` symbol ",")

parseActivator :: Parser String
parseActivator = lexeme $ string "active:" >> manyTill anySingle (try $ lookAhead (symbol "\n" <|> symbol "}"))

--parseModule :: Parser Module
--parseModule = do
--  _ <- lexeme $ string "Module:"
--  _ <- symbol "{"
--  n <- parseName
--  d <- optional parseDescription
--  c <- parseCourses
--  a <- parseActivator
--  cs <- optional parseConstraints
--  subModules <- optional $ do
--    _ <- lexeme $ string "modules:"
--    between (symbol "{") (symbol "}") (parseModule `sepBy` symbol ",")
--  _ <- symbol "}"
--  return Module
--    { name = n
--    , description = maybe "" id d
--    , courses = c
--    , constraints = maybe [] id cs
--    , subModules = maybe [] id subModules
--    , activator = a
--    }
