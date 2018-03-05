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

testClient :: ClientM () -> IO ()
testClient action = do
  -- manager <- newManager defaultManagerSettings
  manager <- newTlsManager
  url <- parseBaseUrl "https://api.binance.com/api/v1"
  print url
  let env = ClientEnv manager url
  print =<< runClientM action env

uselessNumbers :: ClientM ()
uselessNumbers = do
  trades <- getTrades (Just "ETHBTC") (Just 100)
  putTextLn $ show trades

