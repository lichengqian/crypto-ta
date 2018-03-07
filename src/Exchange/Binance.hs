{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Exchange.Binance where

import           Data.Aeson
import           Data.List.NonEmpty (fromList)
import           Data.Proxy
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           Universum
import Text.Read (read)

import MACD
import qualified Data.Text               as T

-- | convert int to LocalTime, for binance
int2LocalTime :: Integer -> IO LocalTime
int2LocalTime n = utcToLocalTime <$> getTimeZone utcTime <*> pure utcTime
  where
    utcTime = posixSecondsToUTCTime . fromIntegral $ div n 1000
 
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

evalClientM :: (MonadIO m, MonadThrow m) => ClientEnv -> ClientM a -> m a
evalClientM env action = liftIO $ do
  runClientM action env >>= \case
      Left err -> throwM err
      Right r  -> return r

binanceEnv = smartClientEnv "https://api.binance.com/api/v1"

getPrices :: Symbol -> Limit -> Maybe FromID -> ClientM (Maybe FromID, [(LocalTime, Double)])
getPrices sym limit mbFromId = do
  trades <- getTrades (Just sym) (Just limit) mbFromId
  prices <- liftIO $ traverse f trades
  return (Just . tid . last . fromList $ trades, prices)
  where
    f Trade{..} = (, read . toString $ price) <$> int2LocalTime time
  -- putTextLn $ show trades

testPrices = getPrices "ETHBTC" 100 Nothing >>= computeRSI 10 . fmap snd . snd >>= print
