

module Main where
  import Web.Scotty
  import qualified Programs
  import Control.Monad.Trans.Class (lift)
  import DSLParser
  import qualified Data.Map as Map
  import qualified Courses
  import Network.Wai (Middleware)
  import Network.Wai.Middleware.Cors (simpleCors, cors, CorsResourcePolicy(..))

  dslFilePath :: String
  dslFilePath = "test_files/full_module.isp"

  main :: IO ()
  main = scotty 3000 $ do
    middleware simpleCors
    get "/run" $ do
      res <- lift $ Programs.runConstraintChecker dslFilePath "ING_CS" "isp1"
      json res

    get "/res" $ do
      res <- lift $ Programs.parseDSL dslFilePath
      json res

    get "/res/mods" $ do
      res <- lift $ do
        r <- Programs.parseDSL dslFilePath
        return r.modules
      json res

    get "/res/isps" $ do
      res <- lift $ do
        r <- Programs.parseDSL dslFilePath
        return r.isps
      json res

    get "/res/courses" $ do
      res <- lift $ do
        r <- Programs.parseDSL dslFilePath
        return r.courses
      json res