module MACD where

import Universum
import Foreign.C.Types
import qualified Data.Vector.Storable as V
import Data.TALib
import GHC.Exts (fromList)
import Test.QuickCheck

instance (V.Storable a, Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fromList <$> arbitrary

data TAException = TAException Text Int
  deriving (Eq, Show)

instance Exception TAException where
  
taException msg = throwM . TAException msg

type Price = [Double]
type Value = V.Vector CDouble

-- | MACD配置参数
data MACDConf = MACDConf
  { fastPeriod :: Int
  , slowPeriod :: Int
  , signalPeriod :: Int
  } deriving (Eq, Show)

-- | MACD 指标
data MACD = MACD
  { macd :: Value
  , macdSignal :: Value
  , macdHist :: Value
  } deriving (Eq, Show)

computeMACD :: (MonadThrow m, MonadIO m) => MACDConf -> Price -> m MACD
computeMACD MACDConf{..} prices = do
    let inReal = fromList $ map CDouble prices
    retE <- liftIO $ ta_macd inReal fastPeriod slowPeriod signalPeriod
    case retE of
      Left err -> taException "MACD" err
      Right (_, _, macd, macdSignal, macdHist) ->
        pure $ MACD{..}


-- | rsi 指标
newtype TimePeriod = TimePeriod Int
  deriving (Eq, Show, Num)

computeRSI :: (MonadThrow m, MonadIO m) => TimePeriod -> Price -> m Value
computeRSI (TimePeriod t) prices = do
    let inReal = fromList $ fmap CDouble prices
    retE <- liftIO $ ta_rsi inReal t
    case retE of
      Left err -> taException "RSI" err
      Right (_, _, vs) -> return vs


_test1 a b c = do
  generate arbitrary >>= computeMACD (MACDConf a b c) 

_test2 a = do
  generate arbitrary >>= computeRSI (TimePeriod a) 
