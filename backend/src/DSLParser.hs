{-# LANGUAGE DataKinds, TypeFamilies, GADTs, FlexibleInstances, TypeOperators, TypeApplications #-}

-- TypeApplications required for the extract @0

module DSLParser where

-- Qualified vs Non-qualified: Qualified makes sure you need the prefix to access the exported functionality. This is not necessary with normal import.

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T -- Qualified
import qualified Data.Map as Map

import qualified Courses
import qualified ISP
import qualified StudyProgram
import qualified Data.Set as Set
import Data.Function (on)
import Data.List (sortOn, groupBy)
import qualified Constraints
import GHC.TypeLits
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Preparation
import Control.Monad.Reader

-- Parsec is the core parser type in Megaparsec. Represents a parser that can consume input and produce a result.
-- Void: error type (don't care about custom error information; TODO later)
-- Text: type of input the parser works on (string/bytestring/text)
-- TODO difference Text and String?
type Parser = Parsec Void Text

data ParseObj = ISPObj ISP.ISP | ModuleObj StudyProgram.ModuleWRef | CourseObj Courses.Course deriving (Show)

data ParseResult = ParseResult
  { isps :: Map.Map String ISP.ISP
  , modules :: Map.Map String StudyProgram.Module
  , courses :: Map.Map String Courses.Course
  } deriving (Show, Generic)

instance Aeson.ToJSON ParseResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

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

parseInteger :: Parser Int
parseInteger = lexeme $ L.signed (return ()) (L.decimal <* notFollowedBy (char '.'))

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
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

--pIdentifier :: Parser String
--pIdentifier = try (
--  do
--    _ <- string "Module"
--    failure "Reserved keyword 'Module' cannot be used as an identifier"
--) <|> pIdentifier

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

parseSubmodules :: Parser [Either String StudyProgram.ModuleWRef]
parseSubmodules = parseListField "modules" $ do {
  choice
    [
      Right <$> parseModule,
      Left <$> identifier
    ]
  }

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

parseSimpleConstraint :: Parser Constraints.Constraint
parseSimpleConstraint = do
  c <- choice [
      parseUnaryConstraint,
      parseIncludedConstraint,
      parseSameYearConstraint,
      parseMinMaxSPConstraint,
      parseRangeSPConstraint,
      parseRemainingSPConstraint,
      parseAllConstraint
    ]
  return c

parseAllConstraint :: Parser Constraints.Constraint
parseAllConstraint = do
  _ <- symbol "All"
  SomeValueCons courseCodeRef (SomeValueCons constraint SomeValueNil) <- parseArgsInBrackets $ SomeParserCons identifier $ SomeParserCons parseConstraint SomeParserNil
  return $ Constraints.AllConstraint courseCodeRef constraint


parseBinaryConstraint :: Parser Constraints.Constraint
parseBinaryConstraint = do
  lhs <- parseSimpleConstraint
  operator <- choice [
    symbol "AND" *> return Constraints.AndConstraint,
    symbol "OR" *> return Constraints.OrConstraint,
    symbol "XOR" *> return Constraints.XorConstraint,
    symbol "NOR" *> return Constraints.NorConstraint,
    symbol "NAND" *> return Constraints.NandConstraint
   ]
  rhs <- parseConstraint
  return $ operator lhs rhs

parseUnaryConstraint :: Parser Constraints.Constraint
parseUnaryConstraint = do
  operator <- symbol "NOT" *> return Constraints.NotConstraint
  constraint <- parseConstraint
  return $ operator constraint

--data SomeParser = forall a. SomeParser (Parser a)
--data SomeValue = forall a. SomeValue a

-- Define type-level lists
data HList (ts :: [*]) where
    HNil :: HList '[]
    HCons :: t -> HList ts -> HList (t ': ts)

-- Define SomeParser and SomeValue for HLists
data SomeParser (ts :: [*]) where
    SomeParserNil :: SomeParser '[]
    SomeParserCons :: Parser a -> SomeParser as -> SomeParser (a ': as)

data SomeValue (ts :: [*]) where
    SomeValueNil :: SomeValue '[]
    SomeValueCons :: a -> SomeValue as -> SomeValue (a ': as) -- It knows that by giving these two types, the result will be in a type-level list

parseArgs :: SomeParser ts -> Parser (SomeValue ts) -- Heterogeneous list (type-level list, we don't know the exact type here). But we do know that the types involved in the list are the same!
parseArgs SomeParserNil = return SomeValueNil
parseArgs (SomeParserCons parser restParsers) = do
  res <- parser
  case restParsers of
    SomeParserNil -> return (SomeValueCons res SomeValueNil)
    _ -> do
      _ <- symbol ","
      restResults <- parseArgs restParsers
      return (SomeValueCons res restResults)

--parseArgs :: [SomeParser] -> Parser [SomeValue]
--parseArgs [] = return []
--parseArgs [SomeParser p] = do
--  res <- p
--  return [SomeValue res]
--
--parseArgs (SomeParser p:ps) = do
--  res <- p
--  _ <- symbol ","
--  rest <- parseArgs ps
--  return (SomeValue res : rest)

parseArgsInBrackets :: SomeParser ts -> Parser (SomeValue ts)
parseArgsInBrackets argParsers = lexeme $ between (char '(') (char ')') $ parseArgs argParsers

parseIncludedConstraint :: Parser Constraints.Constraint
parseIncludedConstraint = do
  _ <- symbol "Included"
  SomeValueCons courseCode SomeValueNil <- parseArgsInBrackets $ SomeParserCons identifier SomeParserNil
  return $ Constraints.IncludedConstraint courseCode

parseSameYearConstraint :: Parser Constraints.Constraint
parseSameYearConstraint = do
  _ <- symbol "SameYear"
  (SomeValueCons code1 (SomeValueCons code2 SomeValueNil)) <- parseArgsInBrackets $ SomeParserCons identifier $ SomeParserCons identifier SomeParserNil
  return $ Constraints.SameYearConstraint code1 code2

parseMinMaxSPConstraint :: Parser Constraints.Constraint
parseMinMaxSPConstraint = do
  minMaxConstraint <- choice [
    symbol "MinSP" *> return Constraints.MinSPConstraint,
    symbol "MaxSP" *> return Constraints.MaxSPConstraint
    ]
  (SomeValueCons sp SomeValueNil) <- parseArgsInBrackets $ SomeParserCons parseInteger SomeParserNil
  return $ minMaxConstraint sp

parseRangeSPConstraint :: Parser Constraints.Constraint
parseRangeSPConstraint = do
  _ <- symbol "RangeSP"
  (SomeValueCons min (SomeValueCons max SomeValueNil)) <- parseArgsInBrackets $ SomeParserCons parseInteger $ SomeParserCons parseInteger SomeParserNil
  return $ Constraints.rangeSPConstraint min max

parseRemainingSPConstraint :: Parser Constraints.Constraint
parseRemainingSPConstraint = do
  _ <- symbol "RemainingSP"
  (SomeValueCons sp SomeValueNil) <- parseArgsInBrackets $ SomeParserCons parseInteger SomeParserNil
  return $ Constraints.RemainingSPConstraint sp

parseConstraint :: Parser Constraints.Constraint
parseConstraint = do
  c <- choice [try parseBinaryConstraint, parseSimpleConstraint]
  return c

parseModuleConstraint :: Parser Constraints.Constraint
parseModuleConstraint = do
  parseObject "ModuleConstraint" $ do
    d <- parseField "description" $ T.pack <$> stringLiteral
    c <- parseField "constraint" parseConstraint
    return $ Constraints.ModuleConstraint d c

parseModule :: Parser StudyProgram.ModuleWRef
parseModule = do
  _ <- spaceConsumer
  parseObject "Module" $ do
    n <- parseName
    d <- optional parseDescription
    c <- optional $ parseListField "courses" identifier
    constraints <- optional $ parseListField "moduleConstraints" parseModuleConstraint
  --  a <- parseActivator
  --  cs <- optional parseConstraints
    subModules <- optional parseSubmodules
    return StudyProgram.ModuleWRef
      {
      StudyProgram.commonFields = StudyProgram.ModuleCommonFields
        {
          StudyProgram.name = n,
          StudyProgram.description = maybe "" id d,
          StudyProgram.courses = maybe [] id c,
          StudyProgram.activator = StudyProgram.trueActivator,
          StudyProgram.constraints = maybe [] id constraints -- TODO fix this hardcoded stuff
        },
      StudyProgram.subModules = maybe [] id subModules
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

parseISPOptions :: Parser ISP.ISPOptions
parseISPOptions = do
  spec <- parseStringField "specialisation"
  bg <- parseStringField "background"
  let ispOptions = Map.fromList[("background", bg), ("specialisation", spec)]
  return ispOptions

parseSet :: (Ord a) => Parser [a] -> Parser (Set.Set a)
parseSet p = Set.fromList <$> p

parseCourseSelection :: Parser ISP.CourseSelection
parseCourseSelection = do
  passed <- optional $ parseSet $ parseListField "passed" $ identifier
  planned <- optional $ parseListField "planned" $ parseSet $ parseList $ identifier -- Contains a list of lists of course identifiers
  return ISP.CourseSelection {
    ISP.passed = maybe Set.empty id passed
  , ISP.planned = maybe [] id planned
  }

parseISP :: Parser ISP.ISP
parseISP = parseObject "ISP" $ do
  studyProgram <- parseIdentifierField "studyProgram"
  ispOptions <- parseISPOptions
  courseSel <- parseField "courseSelection" $ parseNested $ parseCourseSelection
  return ISP.ISP {
    ISP.studyProgram = studyProgram
  , ISP.options = ispOptions
  , ISP.courseSelection = courseSel
  }

parseObjects :: Parser [(String, ParseObj)]
parseObjects = do
  _ <- spaceConsumer
  parsedObjs <- many $ choice
    [ do {
      course <- parseCourse;
      return (course.code, CourseObj course)
    },
    parseAssignment $ do { -- TODO make sure that you can parse in all cases and have proper error handling (no usage of tries because this eliminates errors). E.g. you can't parse something with 'Course' as name
        res <- choice [
          ISPObj <$> parseISP,
          ModuleObj <$> parseModule
        ];
        return res
      }
    ]
  return parsedObjs

createParseResultFromObjs :: [(String, ParseObj)] -> ParseResult
createParseResultFromObjs objs =
  let isps = Map.fromList $ foldr (\(n, obj) acc -> case obj of
                                        ISPObj obj -> ((n, obj) : acc)
                                        _ -> acc
                                        ) [] objs in
  let modulesWRef = Map.fromList $ foldr (\(n, obj) acc -> case obj of
                                        ModuleObj obj -> ((n, obj) : acc)
                                        _ -> acc
                                        ) [] objs in
  let modules = runReader (Preparation.buildModules modulesWRef) $ Preparation.Env { modMap = modulesWRef } in
  let courses = Map.fromList $ foldr (\(n, obj) acc -> case obj of
                                        CourseObj obj -> ((n, obj) : acc)
                                        _ -> acc
                                        ) [] objs in

--  let isps = filter (\(n, obj) -> case obj of
--                                    ISPObj isp -> True
--                                    _ -> False
--                                    ) objs in
--  let modules = filter (\(n, obj) -> case obj of
--                                      ModuleObj mod -> True
--                                      _ -> False
--                                      ) objs in
--  let courses = filter (\(n, obj) -> case obj of
--                                      CourseObj course -> True
--                                      _ -> False
--                                      ) objs in
    ParseResult { isps = isps, modules = modules, courses = courses }
--  let groupedParseObjs = groupBy ((==) `on` )


parse :: Parser ParseResult
parse = do
  parseObjs <- parseObjects
  case (checkRedefinitions parseObjs) of
    Just res -> error $ show res
    Nothing ->
      return $ createParseResultFromObjs parseObjs


createErrorMessage :: [(String, ParseObj)] -> String
createErrorMessage grp =
  let name = fst $ head grp in
  let count = length grp in
    "Redefinitions: the name " ++ name ++ " occurs " ++ show count ++ " times"

checkRedefinitions :: [(String, ParseObj)] -> Maybe [String]
checkRedefinitions pairs =
  let grouped = groupBy ((==) `on` fst) $ sortOn fst pairs in
  let redefined = filter (\grp -> length grp > 1) grouped in
    if (length redefined > 0) then
      Just $ map createErrorMessage redefined
    else Nothing