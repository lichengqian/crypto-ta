{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( main
    -- , startApp
    -- , app
    ) where


-- import           Data.Aeson
-- import           Data.Aeson.TH
import           Diagrams.Backend.Canvas.CmdLine
import           Diagrams.Prelude
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Universum
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time.LocalTime

import Prices(prices,mkDate,filterPrices)

import Exchange.Binance

prices' :: [(LocalTime,Double,Double)]
prices' = filterPrices prices (mkDate 1 1 2006) (mkDate 31 12 2006)

-- | 参考 https://github.com/timbod7/haskell-chart/wiki/example-9
chart = toRenderable $ do
    layoutlr_title .= "Price History"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "price 1" [[ (d,v) | (d,v,_) <- prices']])
    plotLeft (line "price 1.1" [[ (d,v + 2) | (d,v,_) <- prices']])
    plotRight (line "price 2" [[ (d,v) | (d,_,v) <- prices']])

priceChart :: Symbol -> IO _
priceChart sym = do
  env <- binanceEnv
  prices' <- evalClientM env $ getPrices sym 500
  return $ toRenderable $ do
    layoutlr_title .= toString sym
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "price 1" [[ (d,v) | (d,v) <- prices']])
    plotRight (line "price 2" [[ (d,v + 1) | (d,v) <- prices']])


main = do
  env <- defaultEnv vectorAlignmentFns 700 400
  chart <- priceChart "ETHBTC"
  let (diagram :: Diagram B, _) = runBackendR env chart
  mainWith diagram
 
-- b1 = (square 20 :: Diagram B) # lw 0.002

-- main = mainWith (pad 1.1 b1)

{--#
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
--}
