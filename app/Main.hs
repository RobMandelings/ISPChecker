module Main (main) where

import Text.Show.Pretty (ppShow)
import StudyProgram
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Data.Void (Void)
import qualified DSLParser
import qualified Preparation
import qualified Data.Map as Map
import Control.Monad.Reader
import qualified ConstraintChecker as CC

type Parser = Parsec Void Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      content <- T.readFile filePath
      let result = parse DSLParser.parse filePath content
      case result of
        Left err -> putStrLn $ errorBundlePretty err
        Right moduleData ->
          case Map.lookup "abc" moduleData.modules of
            Just modWRef ->
              let mod = runReader (Preparation.buildModule modWRef) $ Preparation.Env { modMap = moduleData.modules } in
              let courseStore = CC.createMapCourseStore $ moduleData.courses in
                case (Map.lookup "isp1" moduleData.isps) of
                  Just isp ->
                    let env = CC.Env { isp, courseStore } in
                    let res = CC.runCheckModule mod env in
                      putStrLn $ (ppShow result) ++ "\n" ++
                      (ppShow moduleData.courses) ++ "\n" ++
                      (ppShow moduleData.isps) ++ "\n" ++
                      "Result: " ++ show res ++ "!"
                  Nothing -> error "no isp with this name found"

            Nothing -> error "hi"
--          putStrLn $ ppShow $ Preparation.buildModule moduleData
--          let redefinitions = DSLParser.checkRedefinitions moduleData in
--            print redefinitions
    _ -> putStrLn "ISPChecker usage: stack run <file-path>"