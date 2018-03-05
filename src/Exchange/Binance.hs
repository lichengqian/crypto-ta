{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Exchange.Binance where

import           Data.Aeson
import           Data.Proxy
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           Universum
import Text.Read (read)

import MACD
import qualified Data.Text               as T

type BinanceAPI =
       "trades" :> QueryParam "symbol" Symbol :> QueryParam "limit" Int :> Get '[JSON] [Trade]
  :<|> "historicalTrades" :> QueryParam "symbol" Symbol :> QueryParam "limit" Int :> QueryParam "fromId" Int :> Get '[JSON] [Trade]

type Symbol = Text
type Limit  = Int
type FromID = Int

data Trade = Trade
  { tid   :: Int
  , price :: Text
  , qty   :: Text
  } deriving (Eq, Show)

instance FromJSON Trade where
  parseJSON (Object o) =
    Trade <$> o .: "id"
          <*> o .: "price"
          <*> o .: "qty"

  parseJSON _ = mzero

binanceAPI :: Proxy BinanceAPI
binanceAPI = Proxy

getTrades :: Maybe Symbol -> Maybe Limit -> ClientM [Trade]
getHistoricalTrades :: Maybe Symbol -> Maybe Limit -> Maybe FromID -> ClientM [Trade]
getTrades :<|> getHistoricalTrades = client binanceAPI

testClient :: String -> ClientM () -> IO ()
testClient url' action = do
  url@(BaseUrl t _ _ _) <- parseBaseUrl url'
  print url
  manager <- case t of
    Http -> newManager defaultManagerSettings
    Https -> newTlsManager
  let env = ClientEnv manager url
  print =<< runClientM action env

runBinance = testClient "https://api.binance.com/api/v1"

getPrices :: ClientM [Double]
getPrices = do
  trades <- getTrades (Just "ETHBTC") (Just 100)
  return $ fmap (read . toString . price) trades
  -- putTextLn $ show trades

testPrices = getPrices >>= computeRSI 10 >>= print
