{-# LANGUAGE TemplateHaskell #-}

import ArbitraryTime
import BusinessWeek(isBusinessHour, isBusinessDay)
import Data.Time
import DueDate
    ( dueDate,
      getValidationErrors,
      Hours,
      ValidationError
      (
        NegativeTurnaround,
        SubmitDateIsBeforeNow,
        SubmitDateIsNotBusinessHour
      )
    )
import Test.QuickCheck.All(quickCheckAll, verboseCheckAll)
import TestHelpers
import Test.QuickCheck (quickCheck)

-- Throughout the tests, I will use the following abbreviations:
-- s = submissionDate
-- t = turnaroundHours

{--------------------------}
{-- PROPERTY BASED TESTS --}
{--------------------------}

zeroIsIdentityForTurnaround :: LocalTime -> Bool
zeroIsIdentityForTurnaround s = isBusinessHour s `implies` (dueDate s 0 `minutePrecisionEquals` s)

distributiveInTurnaround :: LocalTime -> Hours -> Hours -> Bool
distributiveInTurnaround s t1 t2 = dueDate (dueDate s t1) t2 == dueDate s (t1 + t2)

commutativeInTurnAround :: LocalTime -> Hours -> Hours -> Bool
commutativeInTurnAround s t1 t2 = dueDate (dueDate s t1) t2 == dueDate (dueDate s t2) t1

dueDateIsAfterSubmission :: LocalTime -> Hours -> Bool
dueDateIsAfterSubmission s t = dueDate s t > s

dueDateIsBusinessDay :: LocalTime -> Hours -> Bool
dueDateIsBusinessDay s = isBusinessDay . dueDate s

dueDateIsBusinessHour :: LocalTime -> Hours -> Bool
dueDateIsBusinessHour s = isBusinessHour . dueDate s

eightBusinessHoursCorrespondToFullDay :: LocalTime -> Hours -> Bool
eightBusinessHoursCorrespondToFullDay s t =
    let
        distance = distanceInMinutes (dueDate s t) s
        minutesInDay = 24 * 60
    in
        (t `mod` 8 == 0 && isBusinessHour s) `implies` (distance `mod` minutesInDay == 0)

fiveBusinessDaysCorrespondToFullWeek :: LocalTime -> Hours -> Bool
fiveBusinessDaysCorrespondToFullWeek s t =
    let
        distance = distanceInMinutes (dueDate s t) s
        minutesInWeek = 7 * 24 * 60
    in
        (t `mod` (5 * 8) == 0  && isBusinessHour s) `implies` (distance `mod` minutesInWeek == 0)

onlyPastSubmissionDateFails :: LocalTime -> Hours -> LocalTime -> Bool
onlyPastSubmissionDateFails s t now =
    let
        isSubmissionBeforeNow = (now `diffLocalTime` s) > 0
        errors = getValidationErrors s t now
    in
        (SubmitDateIsBeforeNow `elem` errors) `iff` isSubmissionBeforeNow

onlyNegativeTurnaroundFails :: LocalTime -> Hours -> LocalTime -> Bool
onlyNegativeTurnaroundFails s t now = (NegativeTurnaround `elem` getValidationErrors s t now) `iff` (t < 0)

onlyNonBusinessHourSubmissionFails :: LocalTime -> Hours -> LocalTime -> Bool
onlyNonBusinessHourSubmissionFails s t now =
    (SubmitDateIsNotBusinessHour `elem` getValidationErrors s t now) `iff` not (isBusinessHour s)

{------------------------}
{-- SOME EXAMPLE DATES --}
{------------------------}

thursdayNoon :: LocalTime
thursdayNoon = LocalTime {
    localDay = fromGregorian 2022 7 7,
    localTimeOfDay = midday
}

thursdayMidnight :: LocalTime
thursdayMidnight = LocalTime {
    localDay = fromGregorian 2022 7 7,
    localTimeOfDay = midnight
}

fridayNoon :: LocalTime
fridayNoon = LocalTime {
    localDay = fromGregorian 2022 7 8,
    localTimeOfDay = midday
}

saturdayNoon :: LocalTime
saturdayNoon = LocalTime {
    localDay = fromGregorian 2022 7 9,
    localTimeOfDay = midday
}

followingMondayNoon :: LocalTime
followingMondayNoon = LocalTime {
    localDay = fromGregorian 2022 7 11,
    localTimeOfDay = midday
}

{------------------------------}
{-- SOME CONCRETE TEST CASES --}
{------------------------------}

pastSubmissionDateFails :: Bool
pastSubmissionDateFails =
    SubmitDateIsBeforeNow `elem` getValidationErrors thursdayNoon 8 fridayNoon

negativeTurnAroundFails :: Bool
negativeTurnAroundFails = NegativeTurnaround `elem` getValidationErrors thursdayNoon (-10) fridayNoon

nonBusinessHourSubmissionFails :: Bool
nonBusinessHourSubmissionFails =
    SubmitDateIsNotBusinessHour `elem` getValidationErrors saturdayNoon 10 saturdayNoon &&
    SubmitDateIsNotBusinessHour `elem` getValidationErrors thursdayMidnight 10 thursdayNoon

validRequestDoesNotFail :: Bool
validRequestDoesNotFail = null $ getValidationErrors fridayNoon 8 thursdayNoon

nightsAreSkipped :: Bool
nightsAreSkipped = dueDate thursdayNoon 8 `minutePrecisionEquals` fridayNoon

weekendsAreSkipped :: Bool
weekendsAreSkipped = dueDate thursdayNoon (2 * 8) `minutePrecisionEquals` followingMondayNoon

{-----------------}
{-- TEST RUNNER --}
{-----------------}

-- return []
-- runAllTests = $verboseCheckAll

main :: IO ()
main = do
    quickCheck zeroIsIdentityForTurnaround -- OK
    quickCheck distributiveInTurnaround
    quickCheck commutativeInTurnAround
    quickCheck onlyNegativeTurnaroundFails
    quickCheck onlyNonBusinessHourSubmissionFails
    quickCheck onlyPastSubmissionDateFails
    quickCheck eightBusinessHoursCorrespondToFullDay
    quickCheck fiveBusinessDaysCorrespondToFullWeek
    quickCheck pastSubmissionDateFails
    quickCheck negativeTurnAroundFails
    quickCheck nonBusinessHourSubmissionFails
    quickCheck validRequestDoesNotFail
    quickCheck nightsAreSkipped
    quickCheck weekendsAreSkipped