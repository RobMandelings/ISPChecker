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

import qualified Activator
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
-- Void: error type (don't care about custom error information;
-- Text: type of input the parser works on (string/bytestring/text)
type Parser = Parsec Void Text

-- | The results of parsing an entire file is stored in parse objects.
data ParseObj = ISPObj ISP.ISP | ModuleObj StudyProgram.ModuleWRef | CourseObj Courses.Course | ConstraintObj Constraints.Constraint deriving (Show)

-- | Entire parsing result of parsing a single file
data ParseResult = ParseResult
  { isps :: Map.Map String ISP.ISP
  , modules :: Map.Map String StudyProgram.Module
  , courses :: Map.Map String Courses.Course
  , constraints :: Map.Map String Constraints.Constraint
  } deriving (Show, Generic)

-- | Used to convert the type into json format for use in front-end
instance Aeson.ToJSON ParseResult where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

-- | Parses a single line comment
singleLineComment :: Parser ()
singleLineComment = L.skipLineComment "//"

-- | Parses multiple line comments
multiLineComment :: Parser ()
multiLineComment = L.skipBlockComment "/*" "*/"

-- | Used to consume empty spaces (including singleLineComment and multiLineComment). Usually done after parsing meaningful data.
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

-- | Parses a single character (excluding the quotation character)
charLiteralExclQuotation :: Parser Char
charLiteralExclQuotation = satisfy (\c -> c /= '"')

-- | Parses an integer
parseInteger :: Parser Int
parseInteger = lexeme $ L.signed (return ()) (L.decimal <* notFollowedBy (char '.'))

-- | Parses a string literal
stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') (many charLiteralExclQuotation) -- Composed of parsing many char literals


{- | (:) is the cons operator. Adds an element to a list at the front (prepend)
  <$> infix form of fmap (applies function to the result of the functor) -> in this case (:) applied to result of letterChar
  <*> : part of the applicative typeclass. Applies function wrapped in a context to an argument wrapped in a context. In this case its the partially applied (:) function with letterchar to many alphaNumChar.
-}
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-- | Parses a field in the dsl. A field is a combination of key and value pairs
parseField :: Text -> Parser a -> Parser a
parseField fieldName fieldParser = symbol fieldName >> symbol ":" >> (lexeme $ fieldParser) <* symbol ","

-- | Parses a field of type string (a literal string, such as "hello")
parseStringField :: Text -> Parser String
parseStringField fieldName = parseField fieldName stringLiteral

-- | Parses a field that has an identifier as value (e.g. key: hello), instead of literal "hello". This is used to check for references within the dsl (e.g. course identifier that refers a to a course object defined in the DSL
parseIdentifierField :: Text -> Parser String
parseIdentifierField fieldName = parseField fieldName identifier

-- | Parses a field of type list (e.g. [1, 2, 3] where 1, 2, 3 are course identifiers)
parseListField :: Text -> Parser a -> Parser [a]
parseListField fieldName parser = parseField fieldName $ parseList $ parser

-- | Parses a name string field
parseName :: Parser String
parseName = parseField "name" stringLiteral

-- | Parses a description string field
parseDescription :: Parser String
parseDescription = parseField "description" stringLiteral

-- | Parses a list of submodules. A submodule can either be a nested module (which is defined inline) or a reference to a module
-- | Identifiers are used to split module definition into multiple high-level definitions, which makes the code in the DSL much more readable.
parseSubmodules :: Parser [Either String StudyProgram.ModuleWRef]
parseSubmodules = parseListField "modules" $ do {
  choice
    [
      Right <$> parseModule,
      Left <$> identifier
    ]
  }

-- | Parses a list of elements separated by a comma
parseList :: Parser a -> Parser [a]
parseList p = between (symbol "[") (symbol "]") (p `sepBy` symbol ",")

-- | Parses an object with a specific name (e.g. Module, ModuleConstraint, Course, ISP)
parseObject :: Text -> Parser a -> Parser a
parseObject name p = do
  _ <- symbol name
  parseNested p -- No need to use the return keyword because this is already a monadic action that results the result

-- | Parses the object that is within the curly braces
parseNested :: Parser a -> Parser a
parseNested = between (symbol "{") (symbol "}")

-- | Parses a single integer
parseInt :: Parser Int
parseInt = L.decimal

-- | Parses a period (First, Second, AllYear)
parsePeriod :: Parser Courses.Period
parsePeriod = choice
  [ symbol "First" *> return Courses.FirstSem
  ,  symbol "Second" *> return Courses.SecondSem
  ,  symbol "AllYear" *> return Courses.AllYear
  ]

-- | Parses an assignment. Assignments are used to define references to objects in the DSL (at the top level).
-- | These references can then be used in the definition of another object.
parseAssignment :: Parser a -> Parser (String, a)
parseAssignment p = do
  lhs <- identifier
  _ <- symbol "="
  rhs <- p
  return (lhs, rhs)

-- | Parses a constraint that maximally has one nested constraint.
parseSimpleConstraint :: Parser Constraints.Constraint
parseSimpleConstraint = do
  c <- choice [
      parseUnaryConstraint,
      parseIncludedConstraint,
      parseSameYearConstraint,
      parseMinMaxSPConstraint,
      parseRangeSPConstraint,
      parseAllConstraint
    ]
  return c

parseSet :: (Ord a) => Parser [a] -> Parser (Set.Set a)
parseSet p = Set.fromList <$> p

-- | Parses a constraint that requires all courses within the scope to satisfy the constraint (see AllConstraint)
parseAllConstraint :: Parser Constraints.Constraint
parseAllConstraint = do
  _ <- symbol "All"
  SomeValueCons courseCodeRef (SomeValueCons constraint SomeValueNil) <- parseArgsInBrackets $ SomeParserCons identifier $ SomeParserCons parseConstraint SomeParserNil
  return $ Constraints.AllConstraint courseCodeRef constraint

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

-- | Parses the arguments within brackets (including commas). This is a very flexible function that may parse arguments of any type (e.g. SameYear(H04X2A, H04J4A))
parseArgsInBrackets :: SomeParser ts -> Parser (SomeValue ts)
parseArgsInBrackets argParsers = lexeme $ between (char '(') (char ')') $ parseArgs argParsers

-- | Parses a constraint that checks whether a course is included in the ISP
parseIncludedConstraint :: Parser Constraints.Constraint
parseIncludedConstraint = do
  _ <- symbol "Included"
  SomeValueCons courseCode SomeValueNil <- parseArgsInBrackets $ SomeParserCons identifier SomeParserNil
  return $ Constraints.IncludedConstraint courseCode

-- | Parses a constraint that checks whether two courses are included in the same year
parseSameYearConstraint :: Parser Constraints.Constraint
parseSameYearConstraint = do
  _ <- symbol "SameYear"
  (SomeValueCons code1 (SomeValueCons code2 SomeValueNil)) <- parseArgsInBrackets $ SomeParserCons identifier $ SomeParserCons identifier SomeParserNil
  return $ Constraints.SameYearConstraint code1 code2

-- | Parses a constraint that checks whether the study points of a course are within a certain range (depending on what is written in the dsl, this can either be min or max)
parseMinMaxSPConstraint :: Parser Constraints.Constraint
parseMinMaxSPConstraint = do
  minMaxConstraint <- choice [
    symbol "MinSP" *> return Constraints.MinSPConstraint,
    symbol "MaxSP" *> return Constraints.MaxSPConstraint
    ]
  (SomeValueCons sp SomeValueNil) <- parseArgsInBrackets $ SomeParserCons parseInteger SomeParserNil
  return $ minMaxConstraint sp

-- | Parses a constraint that checks whether the study points of a course are within a certain range
parseRangeSPConstraint :: Parser Constraints.Constraint
parseRangeSPConstraint = do
  _ <- symbol "RangeSP"
  (SomeValueCons min (SomeValueCons max SomeValueNil)) <- parseArgsInBrackets $ SomeParserCons parseInteger $ SomeParserCons parseInteger SomeParserNil
  return $ Constraints.rangeSPConstraint min max

-- | Parses a binary constraint (AND, OR, XOR, NOR, NAND)
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

-- | Parses a unary constraint (NOT)
parseUnaryConstraint :: Parser Constraints.Constraint
parseUnaryConstraint = do
  operator <- symbol "NOT" *> return Constraints.NotConstraint
  constraint <- parseConstraint
  return $ operator constraint

-- | Parses a constraint that is scoped to a specific set of courses. This is an explicitly defined scope, scopedConstraints are implicitly added with modules as well.
parseScopedConstraint :: Parser Constraints.Constraint
parseScopedConstraint = do
  scope <- parseList $ identifier
  _ <- symbol "->"
  constraint <- parseConstraint
  return $ Constraints.ScopedConstraint constraint $ Set.fromList scope

-- | Parses any (non-module) constraint
parseConstraint :: Parser Constraints.Constraint
parseConstraint = do
  c <- choice [
    parseScopedConstraint,
    try parseBinaryConstraint, -- explain this in the report: why did you have to add this thing here.
    parseSimpleConstraint]
  return c

-- | Parses a module constraint. ]
-- A module constraint is a constraint that has a description which is shown on the front-end (with given message) and has a nested constraint that is used to actually check the validity of the constraint.
parseModuleConstraint :: Parser Constraints.Constraint
parseModuleConstraint = do
  parseObject "ModuleConstraint" $ do
    d <- parseField "description" $ T.pack <$> stringLiteral
    c <- parseField "constraint" parseConstraint
    return $ Constraints.ModuleConstraint d c

-- | Parses a constraint. A constraint can be any of the following:
parseBinaryActivatorConstraint :: Parser Activator.ActivatorConstraint
parseBinaryActivatorConstraint = do
  lhs <- parseSimpleActivatorConstraint
  operator <- choice [
    symbol "AND" *> return Activator.AndConstraint,
    symbol "OR" *> return Activator.OrConstraint,
    symbol "XOR" *> return Activator.XorConstraint,
    symbol "NOR" *> return Activator.NorConstraint,
    symbol "NAND" *> return Activator.NandConstraint
   ]
  rhs <- parseActivatorConstraint
  return $ operator lhs rhs

-- | Parses a unary activator constraint (NOT)
parseUnaryActivatorConstraint :: Parser Activator.ActivatorConstraint
parseUnaryActivatorConstraint = do
  operator <- symbol "NOT" *> return Activator.NotConstraint
  constraint <- parseActivatorConstraint
  return $ operator constraint

-- | Parses a constraint that checks whether a certain field in the ISP is equal to a certain value
parseEqualActivatorConstraint :: Parser Activator.ActivatorConstraint
parseEqualActivatorConstraint = do
  parseField "activation" $ do
    _ <- symbol "ISP."
    ispRef <- identifier -- E.g. background, specialisation, ...
    _ <- symbol "="
    value <- stringLiteral
    return $ Activator.EqualConstraint ispRef value

-- | Parses a simple activator constraint (either a unary or equal constraint)
parseSimpleActivatorConstraint :: Parser Activator.ActivatorConstraint
parseSimpleActivatorConstraint = do
  c <- choice [
      parseUnaryActivatorConstraint,
      parseEqualActivatorConstraint
    ]
  return c

-- | Parses an activator constraint. An activator constraint is a constraint that is used to check whether a module is activated or not.
parseActivatorConstraint :: Parser Activator.ActivatorConstraint
parseActivatorConstraint = do
  c <- choice [
    try parseBinaryActivatorConstraint, -- Why is this try necessary?
    parseSimpleActivatorConstraint]
  return c

-- | Parses a module. A module is a collection of courses and constraints that are grouped together. Other metadata such as the name and description are also included.
parseModule :: Parser StudyProgram.ModuleWRef
parseModule = do
  _ <- spaceConsumer
  parseObject "Module" $ do
    n <- parseName
    d <- optional parseDescription
    c <- optional $ parseListField "courses" identifier
    constraints <- optional $ parseListField "moduleConstraints" parseModuleConstraint
    activator <- optional $ parseActivatorConstraint
    subModules <- optional parseSubmodules
    return StudyProgram.ModuleWRef
      {
      StudyProgram.commonFields = StudyProgram.ModuleCommonFields
        {
          StudyProgram.name = n,
          StudyProgram.description = maybe "" id d,
          StudyProgram.courses = maybe [] id c,
          StudyProgram.activator = maybe Activator.TrueConstraint id activator,
          StudyProgram.constraints = maybe [] id constraints -- TODO fix this hardcoded stuff
        },
      StudyProgram.subModules = maybe [] id subModules
      }

-- | Parses a course. A course is a single course that is part of the study program. It has a name, code, description, period and study points.
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

-- | Parses the options that are provided in the ISP. These options are specific to the study program and can be used to customise the ISP.
parseISPOptions :: Parser ISP.ISPOptions
parseISPOptions = do
  spec <- parseStringField "specialisation"
  bg <- parseStringField "background"
  let ispOptions = Map.fromList[("background", bg), ("specialisation", spec)]
  return ispOptions

-- | Parses a course selection. A course selection is a collection of courses that are either passed or planned.
parseCourseSelection :: Parser ISP.CourseSelection
parseCourseSelection = do
  passed <- optional $ parseSet $ parseListField "passed" $ identifier
  planned <- optional $ parseListField "planned" $ parseSet $ parseList $ identifier -- Contains a list of lists of course identifiers
  return ISP.CourseSelection {
    ISP.passed = maybe Set.empty id passed
  , ISP.planned = maybe [] id planned
  }

-- | Parses an ISP. An ISP contains a collection of courses that a student has passed or planned. It also contains options that are specific to the study program.
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

-- | Parses a list of objects (e.g. courses, modules, ISPs, constraints). Objects in the dsl can be defined in any order, and references to objects can be used before the object is defined.
parseObjects :: Parser [(String, ParseObj)]
parseObjects = do
  _ <- spaceConsumer
  parsedObjs <- many $ choice
    [ do {
      course <- parseCourse;
      return (course.code, CourseObj course)
    },
    parseAssignment $ do {
        res <- choice [
          ISPObj <$> parseISP,
          ConstraintObj <$> parseModuleConstraint, -- The order has been explicitly thought about because switching Module parsing with ModuleConstraint parsing leads to problem
          ModuleObj <$> parseModule
        ];
        return res
      }
    ]
  return parsedObjs

-- | Creates a parse result from a list of parse objects. The parse result contains a map of ISPs, modules, courses and constraints.
-- | We first need to parse the objects in a list to allow for any order, after which we can create a map for each type of object.
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
  let constraints = Map.fromList $ foldr (\(n, obj) acc -> case obj of
                                          ConstraintObj obj -> ((n, obj) : acc)
                                          _ -> acc
                                          ) [] objs in
    ParseResult { isps = isps, modules = modules, courses = courses, constraints = constraints }


-- | Parses everything into a ParseResult.
parse :: Parser ParseResult
parse = do
  parseObjs <- parseObjects
  case (checkRedefinitions parseObjs) of
    Just res -> error $ show res
    Nothing ->
      return $ createParseResultFromObjs parseObjs

-- | Creates an error message based on the redefinitions that were found.
createErrorMessage :: [(String, ParseObj)] -> String
createErrorMessage grp =
  let name = fst $ head grp in
  let count = length grp in
    "Redefinitions: the name " ++ name ++ " occurs " ++ show count ++ " times"

-- | Checks for redefinitions and returns a list of error messages if redefinitions are found. Otherwise returns Nothing.
checkRedefinitions :: [(String, ParseObj)] -> Maybe [String]
checkRedefinitions pairs =
  let grouped = groupBy ((==) `on` fst) $ sortOn fst pairs in
  let redefined = filter (\grp -> length grp > 1) grouped in
    if (length redefined > 0) then
      Just $ map createErrorMessage redefined
    else Nothing