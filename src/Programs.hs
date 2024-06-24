module Programs where

  import StudyProgram
  import Text.Megaparsec
  import Data.Text (Text)
  import Data.Void (Void)
  import qualified Data.Text.IO as T
  import qualified DSLParser
  import qualified Preparation
  import qualified Data.Map as Map
  import Control.Monad.Reader
  import qualified ConstraintChecker as CC
  import qualified ISP

  parseDSL :: String -> IO DSLParser.ParseResult
  parseDSL filePath = do
    content <- T.readFile filePath
    let result = parse DSLParser.parse filePath content
    case result of
      Left err -> error $ errorBundlePretty err
      Right parseRes -> return parseRes

  runConstraintChecker :: String -> IO CC.CCResult
  runConstraintChecker filePath = do
    parseRes <- parseDSL filePath
    case (Map.lookup "abc" parseRes.modules) of
      Just modWRef ->
        let mod = runReader (Preparation.buildModule modWRef) $ Preparation.Env { modMap = parseRes.modules } in
        let courseStore = CC.createMapCourseStore $ parseRes.courses in
          case (Map.lookup "isp1" parseRes.isps) of
            Just isp ->
              let env = CC.Env { isp, courseStore } in
              let res = CC.runCheckModule mod env in
                case res of
                  Just r -> return r
                  Nothing -> error "Failed to run the module check"
            Nothing -> error "no isp with this name found"