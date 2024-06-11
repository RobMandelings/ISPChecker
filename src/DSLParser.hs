{-# LANGUAGE OverloadedStrings #-}

module DSLParser where

-- Qualified vs Non-qualified: Qualified makes sure you need the prefix to access the exported functionality. This is not necessary with normal import.

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T -- Qualified

import qualified Courses
import qualified ISP
import qualified StudyProgram

-- Parsec is the core parser type in Megaparsec. Represents a parser that can consume input and produce a result.
-- Void: error type (don't care about custom error information; TODO later)
-- Text: type of input the parser works on (string/bytestring/text)
-- TODO difference Text and String?
type Parser = Parsec Void Text

singleLineComment :: Parser ()
singleLineComment = L.skipLineComment "//"

multiLineComment :: Parser ()
multiLineComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 singleLineComment multiLineComment

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
parseField fieldName fieldParser = symbol fieldName >> symbol ":" >> (lexeme $ fieldParser) <* symbol ","

parseStringField :: Text -> Parser String
parseStringField fieldName = parseField fieldName stringLiteral

parseIdentifierField :: Text -> Parser String
parseIdentifierField fieldName = parseField fieldName identifier

parseListField :: Text -> Parser a -> Parser [a]
parseListField fieldName parser = parseField fieldName $ parseList $ parser

parseName :: Parser String
parseName = parseField "name" stringLiteral

parseDescription :: Parser String
parseDescription = parseField "description" stringLiteral

parseConstraints :: Parser [String]
parseConstraints = lexeme $ string "constraints:" >> between (symbol "[") (symbol "]") (identifier `sepBy` symbol ",")

parseSubmodules :: Parser [StudyProgram.Module]
parseSubmodules = parseListField "modules" parseModule

parseList :: Parser a -> Parser [a]
parseList p = between (symbol "[") (symbol "]") (p `sepBy` symbol ",")

parseObject :: Text -> Parser a -> Parser a
parseObject name p = do
  _ <- symbol name
  parseNested p -- No need to use the return keyword because this is already a monadic action that results the result

parseNested :: Parser a -> Parser a
parseNested = between (symbol "{") (symbol "}")

parseComma :: Parser Text
parseComma = symbol ","

parseInt :: Parser Int
parseInt = L.decimal

parsePeriod :: Parser Courses.Period
parsePeriod = choice
  [ symbol "First" *> return Courses.FirstSem
  ,  symbol "Second" *> return Courses.SecondSem
  ,  symbol "AllYear" *> return Courses.AllYear
  ]

--parseActivator :: Parser String
--parseActivator = lexeme $ string "active:" >> manyTill anySingle (try $ lookAhead (symbol "\n" <|> symbol "}"))

parseAssignment :: Parser a -> Parser (String, a)
parseAssignment p = do
  lhs <- identifier
  _ <- symbol "="
  rhs <- p
  return (lhs, rhs)

parseModule :: Parser StudyProgram.Module
parseModule = do
  _ <- spaceConsumer
  parseObject "Module" $ do
    n <- parseName
    d <- optional parseDescription
    c <- optional $ parseListField "courses" identifier
  --  a <- parseActivator
  --  cs <- optional parseConstraints
    subModules <- optional parseSubmodules
    return StudyProgram.Module
      { StudyProgram.name = n
      , StudyProgram.description = maybe "" id d
      , StudyProgram.courses = maybe [] id c
      , StudyProgram.activator = StudyProgram.trueActivator
      , StudyProgram.constraints = []
      , StudyProgram.subModules = maybe [] id subModules
  --      constraints = []
  --    , constraints = maybe [] id cs
  --    , subModules = maybe [] id subModules
      }

parseCourse :: Parser Courses.Course
parseCourse = do
  parseObject "Course" $ do
    n <- parseField "name" stringLiteral
    d <- optional $ parseField "description" stringLiteral
    code <- parseField "code" $ identifier
    period <- parseField "period" $ parsePeriod
    sp <- parseField "studyPoints" $ parseInt
    return Courses.Course {
      Courses.name = n
    , Courses.description = maybe "" id d
    , Courses.code = code
    , Courses.period = period
    , Courses.studyPoints = sp
    }

parseCourseSelection :: Parser [[String]]
parseCourseSelection = do
  passed <- optional $ parseListField "passed" $ identifier
  planned <- optional $ parseListField "planned" $ parseList $ identifier -- Contains a list of lists of course identifiers
  return ((maybe [] id passed) : (maybe [] id planned))

parseISP :: Parser ISP.ISP
parseISP = parseObject "ISP" $ do
  identifier <- parseIdentifierField "studyProgram"
  spec <- parseStringField "specialisation"
  bg <- parseStringField "background"
  courseSel <- parseField "courseSelection" $ parseNested $ parseCourseSelection
  error "Not implemented yet"

data ParseRes = ISPRes ISP.ISP | ModuleRes StudyProgram.Module | CourseRes Courses.Course

parseFile :: Parser ParseRes
parseFile = do
  _ <- spaceConsumer
  error "how"