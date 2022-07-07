module TestHelpers where

import Data.Time 
    ( TimeOfDay(TimeOfDay, todMin, todHour), 
      LocalTime(localTimeOfDay, localDay) 
    )

getMinute :: LocalTime -> Int
getMinute = todMin . localTimeOfDay

getHour :: LocalTime -> Int
getHour = todHour . localTimeOfDay

minutePrecisionEquals :: LocalTime -> LocalTime -> Bool
minutePrecisionEquals a b = localDay a == localDay b && getHour a == getHour b && getMinute a == getMinute b

minutePrecisionEqualsTod :: TimeOfDay -> TimeOfDay -> Bool
minutePrecisionEqualsTod (TimeOfDay h1 m1 s1) (TimeOfDay h2 m2 s2) = h1 == h2 && m1 == m2

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
