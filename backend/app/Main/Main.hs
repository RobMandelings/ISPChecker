module Main (main) where

import Text.Show.Pretty (ppShow)
import StudyProgram
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import Data.Void (Void)
import qualified Programs

type Parser = Parsec Void Text

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      res <- Programs.runConstraintChecker filePath "cs" "isp1"
      putStrLn $ ppShow res
--          putStrLn $ ppShow $ Preparation.buildModule moduleData
--          let redefinitions = DSLParser.checkRedefinitions moduleData in
--            print redefinitions
    _ -> putStrLn "ISPChecker usage: stack run <file-path>"