{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    , main
    ) where


import           Data.Aeson
import           Data.Aeson.TH
import           Diagrams.Backend.Canvas.CmdLine
import           Diagrams.Prelude
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Universum

b1 = (square 20 :: Diagram B) # lw 0.002

main = mainWith (pad 1.1 b1)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
