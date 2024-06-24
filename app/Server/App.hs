module Main where
  import Web.Scotty
  import qualified Programs
  import Control.Monad.Trans.Class (lift)


  main :: IO ()
  main = scotty 3000 $ do
    get "/run" $ do
      res <- lift $ Programs.runConstraintChecker "test_files/simple_module.isp"
      json res

    get "/mod" $ do
      res <- lift $ Programs.parseDSL "test_files/simple_module.isp"
      json res