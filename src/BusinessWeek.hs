module BusinessWeek where

import Control.Applicative(liftA2)
import Data.Time
    ( TimeOfDay(TimeOfDay, todMin, todHour),
      LocalTime(..),
      dayOfWeek,
      DayOfWeek(Monday, Friday) 
    )

-- Repesents a time on the business week storing: 
-- 1) how many working hours elapsed since the start of the business week
-- 2) the minute of the hour
-- E.g. with the business week being Monday-Friday, 9-17, Tuesday 12:15 is 
-- TimeOfBusinessWeek { hourOfBusinessWeek = 11, minuteOfHour = 15 }
data TimeOfBusinessWeek = TimeOfBusinessWeek {
    hourOfBusinessWeek :: Int,
    minuteOfHour :: Int
} deriving Show

{-------------------------------------}
{-- Details of the business week --}
{-------------------------------------}

startOfBusinessWeek  = Monday :: DayOfWeek
endOfBusinessWeek    = Friday :: DayOfWeek
startOfBusinessDay   = TimeOfDay 9 00 00
endOfBusinessDay     = TimeOfDay 17 00 00

lengthOfBusinessDayInHours :: Int
lengthOfBusinessDayInHours = (todHour endOfBusinessDay - todHour startOfBusinessDay) `mod` 24

lengthOfBusinessWeekInDays :: Int
lengthOfBusinessWeekInDays = (fromEnum endOfBusinessWeek - fromEnum startOfBusinessWeek + 1) `mod` 7

lengthOfBusinessWeekInHours :: Int
lengthOfBusinessWeekInHours = lengthOfBusinessWeekInDays * lengthOfBusinessDayInHours

{-----------------------}
{-- Utility functions --}
{-----------------------}

toTimeOfBusinessWeek :: LocalTime -> TimeOfBusinessWeek
toTimeOfBusinessWeek submitDate =
    let
        startOfBusinessWeekDay = fromEnum startOfBusinessWeek
        dayOfBusinessWeek = ((fromEnum . dayOfWeek $ localDay submitDate) - startOfBusinessWeekDay) `mod` 7
        startOfBusinessDayHour = todHour startOfBusinessDay
        hourOfBusinessDay = todHour (localTimeOfDay submitDate) - startOfBusinessDayHour
    in
        TimeOfBusinessWeek {
            hourOfBusinessWeek = dayOfBusinessWeek * lengthOfBusinessDayInHours + hourOfBusinessDay,
            minuteOfHour = todMin (localTimeOfDay submitDate)
        }

fromTimeOfBusinessWeek :: TimeOfBusinessWeek -> (DayOfWeek, TimeOfDay)
fromTimeOfBusinessWeek (TimeOfBusinessWeek hours minutes) =
    let
        dayOfWeek = toEnum ((hours `div` lengthOfBusinessDayInHours) + 1)
        startOfBusinessDayHour = todHour startOfBusinessDay
        hourOfDay = startOfBusinessDayHour + hours `mod` lengthOfBusinessDayInHours
        minuteOfHour = minutes
    in
        (dayOfWeek, TimeOfDay hourOfDay minuteOfHour 00)

-- Adds business hours to a TimeOfBusinessWeek.
-- Returns the new TimeOfBusinessWeek and the number of
-- business weeks between the original and the resulting time.
plus :: TimeOfBusinessWeek -> Int -> (TimeOfBusinessWeek, Int)
plus (TimeOfBusinessWeek hours minutes) turnAround =
    let
        newHours = hours + turnAround
        newTime = TimeOfBusinessWeek (newHours `mod` lengthOfBusinessWeekInHours) minutes
    in (newTime, newHours `div` lengthOfBusinessWeekInHours)

isBusinessDay :: LocalTime -> Bool
isBusinessDay (LocalTime date _) = 
    let
        day = fromEnum $ dayOfWeek date
        weekStartsOn = fromEnum startOfBusinessWeek
        weekEndsOn = fromEnum endOfBusinessWeek
    in
        day >= weekStartsOn && day <= weekEndsOn

isBusinessHour :: LocalTime -> Bool
isBusinessHour date@(LocalTime _ time) =
    let
        hour = todHour time
        dayStartsOn = todHour startOfBusinessDay
        dayEndsOn = todHour endOfBusinessDay
    in
        isBusinessDay date && hour >= dayStartsOn && hour < dayEndsOn