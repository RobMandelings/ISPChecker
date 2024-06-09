{-# LANGUAGE OverloadedStrings #-}

module DSLParser where

-- Qualified vs Non-qualified: Qualified makes sure you need the prefix to access the exported functionality. This is not necessary with normal import.

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T -- Qualified

import StudyProgram

-- Parsec is the core parser type in Megaparsec. Represents a parser that can consume input and produce a result.
-- Void: error type (don't care about custom error information; TODO later)
-- Text: type of input the parser works on (string/bytestring/text)
-- TODO difference Text and String?
type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

{- | Returns a parser that consumes any sequence of characters followed by whitespace.
  Takes a parser as argument and make sure the parser can also parse even though there is trailing whitespace
  The type a could also be an int for example ('parser that produces an int')
-}
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

{- | Creates a parser that produces a specific text
-}
symbol :: Text -> Parser Text
-- L.symbol: takes a whitespace parser and a symbol (string) to parse.
-- So essentially what we do here is currying
symbol = L.symbol spaceConsumer

charLiteralExclQuotation :: Parser Char
charLiteralExclQuotation = satisfy (\c -> c /= '"')

stringLiteral :: Parser String

 -- >> is to discard the first monadic action and return the result of the second monadic acion (chaining two monadic actions and discarding the first one)

-- first version
--stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

stringLiteral = between (char '"') (char '"') (many charLiteralExclQuotation)

{- | (:) is the cons operator. Adds an element to a list at the front (prepend)
  <$> infix form of fmap (applies function to the result of the functor) -> in this case (:) applied to result of letterChar
  <*> : part of the applicative typeclass. Applies function wrapped in a context to an argument wrapped in a context. In this case its the partially applied (:) function with letterchar to many alphaNumChar.
-}
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- TODO allow parsing with spaces between the field and the value

parseField :: Text -> Parser a -> Parser a
parseField fieldName fieldParser = symbol fieldName >> symbol ":" >> (lexeme $ fieldParser) -- <* symbol "," TODO implement the separator later

parseName :: Parser String
parseName = parseField "name" stringLiteral

parseDescription :: Parser String
parseDescription = parseField "description" stringLiteral

parseCourses :: Parser [String]
parseCourses = parseField "courses" (between (symbol "[") (symbol "]") (identifier `sepBy` symbol ","))

parseConstraints :: Parser [String]
parseConstraints = lexeme $ string "constraints:" >> between (symbol "[") (symbol "]") (identifier `sepBy` symbol ",")

parseSubmodules :: Parser [Module]
parseSubmodules = parseField "modules" $ parseNested (parseModule `sepBy` symbol ",")

parseNested :: Parser a -> Parser a
parseNested = between (symbol "{") (symbol "}")

--parseActivator :: Parser String
--parseActivator = lexeme $ string "active:" >> manyTill anySingle (try $ lookAhead (symbol "\n" <|> symbol "}"))

parseModule :: Parser Module
parseModule = do
  _ <- symbol "Module"
  _ <- symbol ":"
  parseNested $ do
    n <- parseName
    d <- optional parseDescription
    c <- parseCourses
  --  a <- parseActivator
  --  cs <- optional parseConstraints
    subModules <- optional parseSubmodules
    return Module
      { name = n
      , description = maybe "" id d
      , courses = c
      , activator = trueActivator
      , constraints = []
      , subModules = maybe [] id subModules
  --      constraints = []
  --    , constraints = maybe [] id cs
  --    , subModules = maybe [] id subModules
      }
