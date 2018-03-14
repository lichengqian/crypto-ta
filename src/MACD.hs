module MACD where

import Universum
import Foreign.C.Types
import qualified Data.Vector.Storable as V
import Data.TALib
import GHC.Exts (fromList)
import Test.QuickCheck
import Types

instance (V.Storable a, Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = fromList <$> arbitrary

data TAException = TAException Text Int
  deriving (Eq, Show)

instance Exception TAException where

taException msg = throwM . TAException msg

toPriceList vs = [p | CDouble p <- V.toList vs]

-- | MACD配置参数
data MACDConf = MACDConf
  { fastPeriod :: Int
  , slowPeriod :: Int
  , signalPeriod :: Int
  } deriving (Eq, Show)

-- | MACD 指标
data MACD = MACD
  { macd :: History Double
  , macdSignal :: History Double
  , macdHist :: History Double
  } deriving (Eq, Show)

inReal :: History Double -> V.Vector CDouble
inReal = fromList . fmap (CDouble . snd) . fromHistory

computeMACD :: (MonadThrow m, MonadIO m) => MACDConf -> History Double -> m MACD
computeMACD MACDConf{..} prices = do
    retE <- liftIO $ ta_macd (inReal prices) fastPeriod slowPeriod signalPeriod
    case retE of
      Left err -> taException "MACD" err
      Right (_, _, macd, macdSignal, macdHist) ->
        pure $ MACD (toHis macd) (toHis macdSignal) (toHis macdHist)
  where
    ts = drop (length prices - slowPeriod - signalPeriod + 2) $ times prices
    toHis = mkHistory ts . toPriceList

-- | rsi 指标
newtype TimePeriod = TimePeriod Int
  deriving (Eq, Show, Num)

computeRSI :: (MonadThrow m, MonadIO m) => TimePeriod -> History Double -> m (History Double)
computeRSI (TimePeriod t) prices = do
    retE <- liftIO $ ta_rsi (inReal prices) t
    case retE of
      Left err -> taException "RSI" err
      Right (_, _, vs) -> return $ mkHistory times $ toPriceList vs
  where
    times = drop t $ fmap fst $ fromHistory prices
{-
_test1 a b c = do
  generate arbitrary >>= computeMACD (MACDConf a b c) 

_test2 a = do
  generate arbitrary >>= computeRSI (TimePeriod a) 
-}
