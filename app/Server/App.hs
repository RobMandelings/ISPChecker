module Main where
  import Web.Scotty

  main :: IO ()
  main = scotty 3000 $ do
    get "/" $ do
      text "Hello, World!"