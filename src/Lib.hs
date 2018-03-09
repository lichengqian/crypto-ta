{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( main
    -- , startApp
    -- , app
    ) where


import           Diagrams.Backend.Canvas
import           Diagrams.Prelude hiding (width, height, Renderable)
import           Universum hiding ((.~))
import           Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import Types
import Exchange.Binance
import MACD
import Graphics.Blank (blankCanvas, send, clearRect, width, height)
import Control.Concurrent (threadDelay, forkIO)

-- | 参考 https://github.com/timbod7/haskell-chart/wiki/example-9
-- 价格绘制柱状图
lineStyle n colour = line_width .~ n
                   $ line_color .~ opaque colour
                   $ def

candle label color vals = liftEC $ do
  plot_candle_line_style  .= lineStyle 1 color
  plot_candle_fill .= True
  plot_candle_rise_fill_style .= solidFillStyle (opaque white)
  plot_candle_fall_fill_style .= solidFillStyle (opaque color)
  plot_candle_tick_length .= 0
  plot_candle_width .= 2
  plot_candle_values .= [ Candle d lo op 0 cl hi | (d,(lo,op,cl,hi)) <- vals]
  plot_candle_title .= label

-- | 工作线程，定时拉取price数据并生成chart
priceChartWorker :: Symbol -> Int -> IO (MVar (Renderable ()))
priceChartWorker sym maxSize = do
  env <- binanceEnv
  mvarChart <- newEmptyMVar
  let loop prices mbLastIndex = do
          (mbLastIndex', prices') <- evalClientM env $ getPrices sym 500 mbLastIndex
          let p = newPrice prices prices'
              cs = toCandle p
          MACD{..} <- computeMACD macdCfg p
          putMVar mvarChart $ toRenderable $ do
            layoutlr_title .= toString sym
            layoutlr_left_axis . laxis_override .= axisGridHide
            layoutlr_right_axis . laxis_override .= axisGridHide
            plotLeft (line "macd 1" [fromHistory macd])
            plotLeft (line "macd 2" [fromHistory macdSignal])
            plotRight (candle "price" red $ fromHistory cs)

          threadDelay (2 * 1000)
          loop p mbLastIndex'

  _ <- forkIO $ loop mempty Nothing
  return mvarChart

  where
    newPrice old new = limitHistory maxSize $ mappend old new
    macdCfg = MACDConf 12 24 50

main :: IO ()
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
