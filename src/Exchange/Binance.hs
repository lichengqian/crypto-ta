{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Exchange.Binance where

import           Data.Aeson
import           Data.List.NonEmpty (fromList)
import qualified Data.Sequence as Seq
import           Data.Proxy
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           Universum
import Text.Read (read)

import MACD
import Types

type BinanceAPI =
       "aggTrades" :> QueryParam "symbol" Symbol :> QueryParam "limit" Int :> QueryParam "fromId" FromID :> Get '[JSON] [Trade]
  :<|> "historicalTrades" :> QueryParam "symbol" Symbol :> QueryParam "limit" Int :> QueryParam "fromId" Int :> Get '[JSON] [Trade]

type Symbol = Text
type Limit  = Int
type FromID = Int

data Trade = Trade
  { tid   :: Int
  , price :: Text
  , qty   :: Text
  , time  :: Integer
  } deriving (Eq, Show)

instance FromJSON Trade where
  parseJSON (Object o) =
    Trade <$> o .: "a"
          <*> o .: "p"
          <*> o .: "q"
          <*> o .: "T"

  parseJSON _ = mzero

binanceAPI :: Proxy BinanceAPI
binanceAPI = Proxy

getTrades :: Maybe Symbol -> Maybe Limit -> Maybe FromID -> ClientM [Trade]
getHistoricalTrades :: Maybe Symbol -> Maybe Limit -> Maybe FromID -> ClientM [Trade]
getTrades :<|> getHistoricalTrades = client binanceAPI

smartClientEnv :: String -> IO ClientEnv
smartClientEnv url' = do
  url@(BaseUrl t _ _ _) <- parseBaseUrl url'
  print url
  manager <- case t of
    Http -> newManager defaultManagerSettings
    Https -> newTlsManager
  return $ ClientEnv manager url

evalClientM :: MonadIO m => ClientEnv -> ClientM a -> m a
evalClientM env action = liftIO $ do
  runClientM action env >>= \case
      Left err -> throwM err
      Right r  -> return r

binanceEnv :: IO ClientEnv
binanceEnv = smartClientEnv "https://api.binance.com/api/v1"

getPrices :: Symbol -> Limit -> Maybe FromID -> ClientM (Maybe FromID, History Double)
getPrices sym limit mbFromId = do
  trades <- getTrades (Just sym) (Just limit) mbFromId
  prices <- liftIO $ traverse f trades
  return (Just . tid . last . fromList $ trades, Seq.fromList prices)
  where
    f Trade{..} = (,) <$> int2LocalTime time <*> pure (read $ toString price)
  -- putTextLn $ show trades

testPrices = getPrices "ETHBTC" 100 Nothing
  >>= computeRSI 10 . snd >>= print
