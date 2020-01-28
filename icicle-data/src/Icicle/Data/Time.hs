{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Icicle.Data.Time (
    Date(..)
  , Time(..)

  -- * System
  , getCurrentDate
  , midnight
  , exclusiveSnapshotTime

  -- * Extract and conversion
  , julianDay
  , gregorianDay
  , timeOfDay
  , localHour
  , localMinute
  , localSecond
  , daysCountIvory
  , secondsCountIvory
  , dateOfText
  , timeOfText
  , dateOfYMD
  , timeOfYMD
  , timeOfDays
  , timeOfIvorySeconds
  , withinWindow
  , unsafeDateOfYMD
  , unsafeTimeOfYMD
  , dayOf
  , monthOf
  , yearOf

  -- * Operations
  , daysDifference
  , secondsDifference
  , minusSeconds
  , minusMonths
  , minusDays
  , packedOfTime
  , timeOfPacked

  -- * Parsing and Printing
  , renderDate
  , renderTime
  , pTime
  , pDate

  , renderOutputTime
  ) where

import           Icicle.Common.NanEq
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Attoparsec.Text

import qualified Data.Time          as C
import qualified Data.Thyme         as Thyme
import qualified Data.Thyme.Time    as Thyme
import           Data.AffineSpace
import           Data.Text  as T
import           Data.Word (Word64)
import           Data.Bits

import           Control.Lens ((^.))

import           P


newtype Date =
  Date {
      getDate :: Thyme.Day
    } deriving (Eq, Ord, Show)

newtype Time =
  Time {
      getDateTime :: Thyme.UTCTime
    } deriving (Eq, Ord)

-- deepseq stops here, it shouldn't matter too much, we don't particularly
-- care about values
instance NFData Time where rnf _ = ()

instance NanEq Time where
  nanEq =
    (==)

instance NanEq Date where
  nanEq =
    (==)

instance Show Time where
 showsPrec p x
  = let g = gregorianDay x
        h = timeOfDay    x
    in showParen (p > 10)
     $ showString "Time ("
     . showsPrec 11 (g ^. Thyme._ymdYear)
     . showString " "
     . showsPrec 11 (g ^. Thyme._ymdMonth)
     . showString " "
     . showsPrec 11 (g ^. Thyme._ymdDay)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todHour)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todMin)
     . showString " "
     . showsPrec 11 (h ^. Thyme._todSec)
     . showString ")"

--------------------------------------------------------------------------------

julianDay :: Time -> Thyme.Day
julianDay x
  = getDateTime x ^. Thyme._utctDay

gregorianDay :: Time -> Thyme.YearMonthDay
gregorianDay x
  = julianDay x ^. Thyme.gregorian

diffTime :: Time -> Thyme.DiffTime
diffTime x
  = getDateTime x ^. Thyme._utctDayTime

timeOfDay :: Time -> Thyme.TimeOfDay
timeOfDay x
  = diffTime x ^. Thyme.timeOfDay

dayOf :: Time -> Int
dayOf x = gregorianDay x ^. Thyme._ymdDay

monthOf :: Time -> Int
monthOf x = gregorianDay x ^. Thyme._ymdMonth

yearOf :: Time -> Int
yearOf x = gregorianDay x ^. Thyme._ymdYear

localHour :: Time -> Int
localHour x = timeOfDay x ^. Thyme._todHour

localMinute :: Time -> Int
localMinute x = timeOfDay x ^. Thyme._todMin

localSecond :: Time -> Int
localSecond x = truncate (timeOfDay x ^. Thyme._todSec)

-- | Number of days since Ivory epoch
daysCountIvory :: Time -> Int
daysCountIvory d
  = Thyme.toModifiedJulianDay (julianDay d) - Thyme.toModifiedJulianDay ivoryEpoch

-- | Number of seconds since Ivory epoch
secondsCountIvory :: Time -> Int
secondsCountIvory t
  = let days  = daysCountIvory  t
        hours = localHour       t
        mins  = localMinute     t
        secs  = localSecond     t
    in (days * 24 * 3600) + (hours * 3600) + (mins * 60) + secs

ivoryEpoch :: Thyme.Day
ivoryEpoch =
  Thyme.ModifiedJulianDay (-94493) -- 1600-03-01

--------------------------------------------------------------------------------

timeOfDays :: Int -> Time
timeOfDays d
 = Time
 $ flip Thyme.mkUTCTime 0
 $ Thyme.ModifiedJulianDay d

unsafeTimeOfYMD :: Int -> Int -> Int -> Time
unsafeTimeOfYMD y m d
 = Time
 $ flip Thyme.mkUTCTime 0
 $ Thyme.fromGregorian y m d

timeOfYMD :: Int -> Int -> Int -> Maybe Time
timeOfYMD y m d
 =   Time
 .   flip Thyme.mkUTCTime 0
 <$> Thyme.fromGregorianValid y m d

timeOfIvorySeconds :: Int64 -> Time
timeOfIvorySeconds seconds =
  Time $ Thyme.addUTCTime (fromIntegral seconds) (Thyme.mkUTCTime ivoryEpoch 0)

timeOfYMDHMS :: Int -> Int -> Int -> Int -> Int -> Int -> Time
timeOfYMDHMS year month day hour minute sec
  = let ymd = Thyme.fromGregorian year month day
        hms = Thyme.timeOfDayToTime
            $ Thyme.TimeOfDay hour minute
            $ Thyme.secondsToDiffTime
            $ fromIntegral sec
    in Time $ Thyme.mkUTCTime ymd hms

-- Unpack the word into an icicle Time
timeOfPacked :: Word64 -> Time
timeOfPacked d
 = let y  = shift (fromIntegral d) (-48)
       m  = shift (fromIntegral d) (-40) .&. 0xff
       d' = shift (fromIntegral d) (-32) .&. 0xff
       i  =       (fromIntegral d)       .&. 0xffffffff
       h  = i `quot` 3600
       m' = i `rem`  3600 `quot` 60
       s  = i `rem`  60
   in timeOfYMDHMS y m d' h m' s

-- Pack into Ivory's date/time format (for use in Sea evaluation).
-- A packed long
--   16 bits: year represented as a short
--   8 bits:  month represented as a byte
--   8 bits:  day represented as a byte
--   32 bits: seconds since start of day
packedOfTime :: Time -> Word64
packedOfTime t@(gregorianDay -> d)
  =  shift (fromIntegral (Thyme.ymdYear  d)) 48
 .|. shift (fromIntegral (Thyme.ymdMonth d)) 40
 .|. shift (fromIntegral (Thyme.ymdDay   d)) 32
 .|. (fromIntegral (3600 * localHour t + 60 * localMinute t + localSecond t))

--------------------------------------------------------------------------------

getCurrentDate :: MonadIO m => m Date
getCurrentDate =
 Date . Thyme.utctDay . Thyme.unUTCTime <$> liftIO Thyme.getCurrentTime

midnight :: Date -> Time
midnight (Date date) =
  Time $ Thyme.mkUTCTime date 0

exclusiveSnapshotTime :: Date -> Time
exclusiveSnapshotTime (Date date) =
  Time $ Thyme.mkUTCTime (Thyme.addDays 1 date) 0

dateOfYMD :: Int -> Int -> Int -> Maybe Date
dateOfYMD y m d =
  Date <$> Thyme.fromGregorianValid y m d

unsafeDateOfYMD :: Int -> Int -> Int -> Date
unsafeDateOfYMD y m d =
  Date $ Thyme.fromGregorian y m d

pDate :: Parser Date
pDate = do
  let
    dash =
      () <$ char '-'
  mdate <- dateOfYMD <$> decimal <* dash <*> decimal <* dash <*> decimal
  maybe (fail "Invalid date") pure mdate

dateOfText :: Text -> Maybe Date
dateOfText =
  rightToMaybe . parseOnly pDate

--------------------------------------------------------------------------------

renderDate  :: Date -> Text
renderDate = T.pack . C.showGregorian . Thyme.fromThyme . getDate

renderTime  :: Time -> Text
renderTime = T.pack . C.showGregorian . Thyme.fromThyme . julianDay

renderOutputTime  :: Time -> Text
renderOutputTime t
 = let fmt = "%Y-%m-%dT%H:%M:%SZ"
       t' = Thyme.fromThyme (getDateTime t) :: C.UTCTime
       str = C.formatTime C.defaultTimeLocale fmt t'
   in  T.pack str

pTime :: Parser Time
pTime
 = (maybe (fail "Invalid time") pure) =<< timeOfYMD <$> decimal <* dash <*> decimal <* dash <*> decimal
   where
    dash :: Parser ()
    dash = () <$ char '-'

timeOfText :: Text -> Maybe Time
timeOfText txt =
  case parseOnly pTime txt of
    Left  _ -> Nothing
    Right x -> Just x

-- | Check whether two given times are within a days window
withinWindow :: Time -> Time -> Int -> Bool
withinWindow fact now window
 = let diff =  daysDifference fact now
   in  diff <= window

-- | Find number of days between two times
daysDifference :: Time -> Time -> Int
daysDifference fact now
  = daysCountIvory now - daysCountIvory fact

-- | Numer of seconds between two times
secondsDifference :: Time -> Time -> Int
secondsDifference fact now
  = let t :: Double
        t = Thyme.toSeconds $ getDateTime now .-. getDateTime fact
    in round t

minusSeconds :: Time -> Int -> Time
minusSeconds d i
 = Time $ getDateTime d .-^ Thyme.fromSeconds i

minusDays :: Time -> Int -> Time
minusDays d i
 = Time
 $ flip Thyme.mkUTCTime (Thyme.utctDayTime view)
 $ Thyme.addDays (-i)   (Thyme.utctDay     view)
 where
   view = getDateTime d ^. Thyme.utcTime

minusMonths :: Time -> Int -> Time
minusMonths d i
 = Time
 $ flip Thyme.mkUTCTime              (Thyme.utctDayTime view)
 $ Thyme.addGregorianMonthsClip (-i) (Thyme.utctDay     view)
 where
   view = getDateTime d ^. Thyme.utcTime
