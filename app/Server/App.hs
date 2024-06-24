

module Main where
  import Web.Scotty
  import qualified Programs
  import Control.Monad.Trans.Class (lift)
  import DSLParser
  import qualified Data.Map as Map
  import qualified Courses


  main :: IO ()
  main = scotty 3000 $ do
    get "/run" $ do
      res <- lift $ Programs.runConstraintChecker "test_files/simple_module.isp"
      json res

    get "/res" $ do
      res <- lift $ Programs.parseDSL "test_files/simple_module.isp"
      json res

    get "/res/mods" $ do
      res <- lift $ do
        r <- Programs.parseDSL "test_files/simple_module.isp"
        return r.modules
      json res

    get "/res/isps" $ do
      res <- lift $ do
        r <- Programs.parseDSL "test_files/simple_module.isp"
        return r.isps
      json res

    get "/res/courses" $ do
      res <- lift $ do
        r <- Programs.parseDSL "test_files/simple_module.isp"
        return r.courses
      json res