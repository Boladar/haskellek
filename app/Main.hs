{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Network.Wai
import Network.HTTP.Types (status200, status404)
import Network.Wai.Handler.Warp (run)
import GHC.Generics

import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.ByteString.Lazy (fromStrict)

data Greeting = Greeting { name :: String} deriving (Generic,Show)

instance FromJSON Greeting

main :: IO ()
main = do
    putStrLn "Starting server on port 3000"
    run 3000 app
    return ()

app :: Application
app request respond = 
    case rawPathInfo request of
        "/hello" -> helloHandler request respond
        _ -> notFoundHandler request respond

helloHandler :: Application
helloHandler request respond = do
    body <- requestBody request
    let maybeGreeting = decode $ fromStrict body :: Maybe Greeting
    case maybeGreeting of
        Just greeting -> respond $ responseLBS status200 [("Content-Type", "text/plain")] (pack $ "Hello, " ++ name greeting)
        Nothing -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Could not parse request body"

notFoundHandler :: Application
notFoundHandler _ respond = respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Not found"
