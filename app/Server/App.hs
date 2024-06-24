module Main where
  import Web.Scotty
  import Data.Aeson
  import qualified Programs

  main :: IO ()
  main = scotty 3000 $ do
    get "/" $ do
      text "Hello, World!"