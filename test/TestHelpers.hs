module TestHelpers where

import Data.Fixed (mod')
import Data.Time(NominalDiffTime, nominalDiffTimeToSeconds, nominalDay)

implies :: Bool -> Bool -> Bool
implies a b = not a || b

iff :: Bool -> Bool -> Bool
iff a b = (a `implies` b) && (b `implies` a)

isMultipleOfWholeDay :: NominalDiffTime -> Bool
isMultipleOfWholeDay difference
    = nominalDiffTimeToSeconds difference `mod'` nominalDiffTimeToSeconds nominalDay == 0

isMultipleOfWholeWeek :: NominalDiffTime -> Bool
isMultipleOfWholeWeek difference
    = nominalDiffTimeToSeconds difference `mod'` nominalDiffTimeToSeconds (7 * nominalDay) == 0
