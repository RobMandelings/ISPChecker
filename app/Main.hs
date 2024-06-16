module Main (main) where


import StudyProgram
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Data.Void (Void)
import qualified DSLParser

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
          putStrLn $ show moduleData
--          let redefinitions = DSLParser.checkRedefinitions moduleData in
--            print redefinitions
    _ -> putStrLn "ISPChecker usage: stack run <file-path>"