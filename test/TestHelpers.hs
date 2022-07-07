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
minutePrecisionEquals a b = localDay a == localDay b && getHour a == getHour b && getMinute a == getMinute b

resolutionInSeconds :: Int
resolutionInSeconds = 60

implies :: Bool -> Bool -> Bool
implies a b = not a || b

iff :: Bool -> Bool -> Bool
iff a b = (a `implies` b) && (b `implies` a)

distanceInMinutes :: LocalTime -> LocalTime -> Int
distanceInMinutes a b =
    let
        dayDifference = fromEnum (localDay a) - fromEnum (localDay b)
        hourDifference = getHour a - getHour b
        minuteDifference = getMinute a - getMinute b
    in
        abs (dayDifference * 24 * 60 + hourDifference * 60 + minuteDifference)

isMultipleOfWholeDay :: NominalDiffTime -> Bool
isMultipleOfWholeDay difference =
    let
        (diffSeconds, _) = properFraction difference
        oneDayInSeconds = 60 * 60 * 24
    in
        abs (oneDayInSeconds - diffSeconds) < 60

isMultipleOfWholeWeek :: NominalDiffTime -> Bool
isMultipleOfWholeWeek difference
    = floor (nominalDiffTimeToSeconds difference) `mod` floor (nominalDiffTimeToSeconds (7 * nominalDay)) < resolutionInSeconds
