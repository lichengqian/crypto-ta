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
import Diagrams.Backend.CmdLine hiding (width, height)
import           Diagrams.Backend.Canvas
import           Diagrams.Backend.Canvas.CmdLine
import           Diagrams.Prelude hiding (width, height)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Universum
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Time.LocalTime

import Prices(prices,mkDate,filterPrices)

import Exchange.Binance
import MACD
import Graphics.Blank (blankCanvas, send, clearRect, width, height)
import Control.Concurrent (threadDelay, forkIO)

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

samplePrices :: [(LocalTime, a)] -> [(LocalTime, a)]
samplePrices = go []
  where
    go :: [(LocalTime, a)] -> [(LocalTime, a)] -> [(LocalTime, a)]

    go acc [] = reverse acc
    go [] (e:xs) = go [e] xs
    go acc@((t, _):_) (e@(t2, _):xs)
      | t == t2 = go acc xs
      | otherwise = go (e:acc) xs

-- | 工作线程，定时拉取price数据并生成chart
priceChartWorker :: Symbol -> Int -> IO (MVar _)
priceChartWorker sym maxSize = do
  env <- binanceEnv
  mvarChart <- newEmptyMVar
  let loop prices mbLastIndex = do
          (mbLastIndex', prices') <- evalClientM env $ getPrices sym 500 mbLastIndex
          let p = newPrice prices $ samplePrices prices'
          MACD{..} <- computeMACD macdCfg $ fmap snd p
          let macd1 = zipWith (\(t, _) p -> (t, p)) p macd
              macd2 = zipWith (\(t, _) p -> (t, p)) p macdSignal
          putMVar mvarChart $ toRenderable $ do
            layoutlr_title .= toString sym
            layoutlr_left_axis . laxis_override .= axisGridHide
            layoutlr_right_axis . laxis_override .= axisGridHide
            plotLeft (line "macd 1" [macd1])
            plotLeft (line "macd 2" [macd2])
            plotRight (line "price" [p])

          threadDelay (2 * 1000)
          loop p mbLastIndex'

  forkIO $ loop [] Nothing
  return mvarChart

  where
    newPrice old new = if n <= maxSize then p else drop (n - maxSize) p
      where
        p = old ++ new 
        n = length p

    macdCfg = MACDConf 12 24 50
 
{-
main = mainWith $ \sym -> do
  env <- defaultEnv vectorAlignmentFns 700 400
  chart <- priceChart $ fromString sym
  let (diagram :: Diagram B, _) = runBackendR env chart
  return diagram
-}

main = do
  let sym = "ETHBTC"
  mvarChart <- priceChartWorker sym 5000
  backendEnv <- defaultEnv vectorAlignmentFns 700 400
  let opts = CanvasOptions $ mkSizeSpec2D (Just 700) (Just 400)
  let loop context = do
        chart <- takeMVar mvarChart
        let (diagram :: Diagram B, _) = runBackendR backendEnv chart
        send context $ do
            clearRect (0,0,width context,height context)
            renderDia Canvas opts diagram

        loop context

  blankCanvas 3000 loop

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
