module TestHelpers where

import Data.Fixed (mod')
import Data.Time(
    localDay,
    localTimeOfDay,
    todHour,
    todMin,
    NominalDiffTime,
    nominalDiffTimeToSeconds,
    nominalDay, LocalTime, TimeOfDay
    )

getMinute :: LocalTime -> Int
getMinute = todMin . localTimeOfDay

getHour :: LocalTime -> Int
getHour = todHour . localTimeOfDay

minutePrecisionEquals :: LocalTime -> LocalTime -> Bool
minutePrecisionEquals a b = (localDay a == localDay b) && (getHour a == getHour b) && (getMinute a == getMinute b)

resolutionInSeconds :: Int
resolutionInSeconds = 60

implies :: Bool -> Bool -> Bool
implies a b = not a || b

iff :: Bool -> Bool -> Bool
iff a b = (a `implies` b) && (b `implies` a)

isMultipleOfWholeDay :: NominalDiffTime -> Bool
isMultipleOfWholeDay difference
    = nominalDiffTimeToSeconds difference `mod'` nominalDiffTimeToSeconds nominalDay < fromIntegral resolutionInSeconds

isMultipleOfWholeWeek :: NominalDiffTime -> Bool
isMultipleOfWholeWeek difference
    = nominalDiffTimeToSeconds difference `mod'` nominalDiffTimeToSeconds (7 * nominalDay) < fromIntegral resolutionInSeconds
