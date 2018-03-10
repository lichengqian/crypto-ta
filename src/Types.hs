module Types where

import           Data.List (groupBy)
import           Data.List.NonEmpty (fromList)
import           Data.Sequence (Seq(..), (<|), (|>))
import qualified Data.Sequence as Seq
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Universum

-- | convert int to LocalTime, for binance
int2LocalTime :: Integer -> IO LocalTime
int2LocalTime n = utcToLocalTime <$> getTimeZone utcTime <*> pure utcTime
  where
    utcTime = posixSecondsToUTCTime . fromIntegral $ div n 1000

minute :: LocalTime -> LocalTime
minute (LocalTime d (TimeOfDay h m _)) = LocalTime d (TimeOfDay h m 0)

type History a = Seq (LocalTime, a)

mkHistory :: [LocalTime] -> [a] -> History a
mkHistory ts xs = Seq.fromList $ zip ts xs

fromHistory :: History a -> [(LocalTime, a)]
fromHistory = toList

times :: History a -> [LocalTime]
times = fmap fst . toList

limitLength :: Int -> Seq a -> Seq a
limitLength maxSize hs
  | Seq.length hs < maxSize = hs
  | otherwise = Seq.drop (maxSize - Seq.length hs) hs

{--
-- | 去掉重复的时间
sampleHistory :: History a -> History a
sampleHistory (History xs) = History $ foldr go [] xs
  where
    go :: Timed a -> [Timed a] -> [Timed a]

    go e [] = [e]
    go e@(Timed t _) (e2@(Timed t2 _):xs)
      | t == t2 = e2:xs
      | otherwise = e:e2:xs
--}

-- | 生成柱状图
toCandle :: Ord a => History a -> History (a, a, a, a)
toCandle his = Seq.fromList $ fmap oneCandle gcs
  where
    xs = fmap (first minute) $ fromHistory his
    gcs = groupBy ((==) `on` fst) xs
    oneCandle xxs = (t, (lo, op, cl, hi))
      where
        t = fst . head . fromList $ xxs
        lo = minimum $ fmap snd xxs
        hi = maximum $ fmap snd xxs
        op = snd . head . fromList $ xxs
        cl = snd . last . fromList $ xxs
