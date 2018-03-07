module Types where

import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Universum

-- | convert int to LocalTime, for binance
int2LocalTime :: Integer -> IO LocalTime
int2LocalTime n = utcToLocalTime <$> getTimeZone utcTime <*> pure utcTime
  where
    utcTime = posixSecondsToUTCTime . fromIntegral $ div n 1000

data Timed a = Timed !LocalTime !a deriving (Show, Eq, Functor)

fromTimed :: Timed a -> a
fromTimed (Timed _ a) = a

newtype History a = History [Timed a] deriving (Eq, Show, Semigroup, Monoid)

instance Functor History where
  fmap f (History xs) = History $ fmap (fmap f) xs

mkHistory :: [LocalTime] -> [a] -> History a
mkHistory ts xs = History $ zipWith Timed ts xs

fromHistory :: History a -> [(LocalTime, a)]
fromHistory (History xs) = fmap (\(Timed t v) -> (t, v)) xs

times :: History a -> [LocalTime]
times (History xs) = fmap (\(Timed t _) -> t) xs

limitHistory maxSize (History xs)
  | length xs < maxSize = History xs
  | otherwise = History (drop (maxSize - length xs) xs)

-- | 去掉重复的时间
sampleHistory :: History a -> History a
sampleHistory (History xs) = History $ foldr go [] xs
  where
    go :: Timed a -> [Timed a] -> [Timed a]

    go e [] = [e]
    go e@(Timed t _) (e2@(Timed t2 _):xs)
      | t == t2 = e2:xs
      | otherwise = e:e2:xs

