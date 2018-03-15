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

-- | MACD指标
data MACD= MACD
  { macdFast :: Int
  , macdSlow :: Int
  , macdSignal :: Int
  } deriving (Eq, Show)

inReal :: History Double -> V.Vector CDouble
inReal = fromList . fmap (CDouble . snd) . fromHistory

instance Computable MACD where
  type Input MACD = History Double
  type Result MACD = History (Double, Double)

  compute MACD{..} prices = do
    retE <- liftIO $ ta_macd (inReal prices) macdFast macdSlow macdSignal
    case retE of
      Left err -> taException "MACD" err
      Right (_, _, macd, macdSignal', macdHist) ->
        pure $ mkHistory ts $ zip (toPriceList macd) (toPriceList macdSignal') 
    where
      ts = drop (length prices - macdSlow - macdSignal + 2) $ times prices

-- | rsi 指标
data RSI = RSI
  { rsiFast  :: Int
  , rsiSlow  :: Int
  } deriving (Eq, Show)

instance Computable RSI where
  type Input RSI = History Double
  type Result RSI = History (Double, Double)

  compute (RSI f s) prices = do
    retF <- singleRSI f
    retS <- singleRSI s

    return $ mkHistory (drop s ts) $ zip (drop (s - f) retF) retS

    where
      ts = times prices
      p = inReal prices
      singleRSI t = do
        retE <- liftIO $ ta_rsi p t
        case retE of
          Left err -> taException "RSI" err
          Right (_, _, vs) -> return $ take (length ts - t) $ toPriceList vs
