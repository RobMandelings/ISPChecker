module Main where
  import Web.Scotty
  import qualified Programs
  import Control.Monad.Trans.Class (lift)


  main :: IO ()
  main = scotty 3000 $ do
    get "/" $ do
      res <- lift $ Programs.runConstraintChecker "test_files/simple_module.isp"
      json res