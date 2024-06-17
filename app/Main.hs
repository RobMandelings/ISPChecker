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
            Just mod ->
              let result = runReader (Preparation.buildModule mod) $ Preparation.Env { modMap = moduleData.modules } in
                putStrLn $ ppShow result

            Nothing -> error "hi"
--          putStrLn $ ppShow $ Preparation.buildModule moduleData
--          let redefinitions = DSLParser.checkRedefinitions moduleData in
--            print redefinitions
    _ -> putStrLn "ISPChecker usage: stack run <file-path>"