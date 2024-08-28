module Programs where

  import StudyProgram
  import Text.Megaparsec
  import Data.Text (Text)
  import Data.Void (Void)
  import qualified Data.Text.IO as T
  import qualified DSLParser

  import qualified Data.Map as Map
  import qualified Data.Map.Strict as StrictMap
  import Control.Monad.Reader
  import qualified ConstraintChecker as CC
  import qualified ISP

  -- | Pares the DSL code within a file and returns the result
  parseDSL :: String -> IO DSLParser.ParseResult
  parseDSL filePath = do
    content <- T.readFile filePath
    let result = parse DSLParser.parse filePath content
    case result of
      Left err -> error $ errorBundlePretty err
      Right parseRes ->
        return parseRes -- The result of parsing the entire file

  -- | Given a file path, module name and name of isp (references to objects within the dsl code), run the constraint checker and return the result for the top-level module (e.g. the study program)
  runConstraintChecker :: String -> String -> String -> IO CC.ModuleResult
  runConstraintChecker filePath moduleName ispName = do
    parseRes <- parseDSL filePath
    case (Map.lookup moduleName parseRes.modules) of
      Just mod ->
        let courseStore = CC.createMapCourseStore $ parseRes.courses in
          case (Map.lookup ispName parseRes.isps) of
            Just isp ->
              let scope = CC.getScope mod isp in
              let env = CC.Env { isp, courseStore, scope} in
              let res = CC.runCheckModule mod env in
                case res of
                  Just r -> return r
                  Nothing -> error "Failed to run the module check"
            Nothing -> error "no isp with this name found"
      Nothing -> error "no module found"