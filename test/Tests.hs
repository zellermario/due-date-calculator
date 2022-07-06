{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Data.Time
import DueDate
    ( dueDate,
      getValidationErrors,
      isBusinessHour,
      isBusinessDay,
      Hours,
      ValidationError 
      (
        NegativeTurnaround, 
        SubmitDateIsBeforeNow, 
        SubmitDateIsNotBusinessHour 
      ) 
    )
import Test.QuickCheck.All(quickCheckAll)
import TestHelpers

-- Throughout the tests, I will use the following abbreviations:
-- s = submissionDate
-- t = turnaroundHours

{--------------------------}
{-- PROPERTY BASED TESTS --}
{--------------------------}

zeroIsIdentityForTurnaround :: LocalTime -> Bool
zeroIsIdentityForTurnaround s = dueDate s 0 == s

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
        difference = dueDate s t `diffLocalTime` s
    in
        (t `mod` 8 == 0) `implies` isMultipleOfWholeDay difference

fiveBusinessDaysCorrespondToFullWeek :: LocalTime -> Hours -> Bool
fiveBusinessDaysCorrespondToFullWeek s t =
    let
        difference = dueDate s t `diffLocalTime` s
    in
        (t `mod` (5 * 8) == 0) `implies` isMultipleOfWholeWeek difference

onlyPastSubmissionDateFails :: LocalTime -> Hours -> UTCTime -> Bool
onlyPastSubmissionDateFails s t now =
    let
        isSubmissionBeforeNow = (utcToLocalTime utc now `diffLocalTime` s) > 0
        errors = getValidationErrors s t now
    in
        (SubmitDateIsBeforeNow `elem` errors) `iff` isSubmissionBeforeNow

onlyNegativeTurnaroundFails :: LocalTime -> Hours -> UTCTime -> Bool
onlyNegativeTurnaroundFails s t now = (NegativeTurnaround `elem` (getValidationErrors s t now)) `iff` (t < 0)

onlyNonBusinessHourSubmissionFails :: LocalTime -> Hours -> UTCTime -> Bool
onlyNonBusinessHourSubmissionFails s t now =
    (NegativeTurnaround `elem` getValidationErrors s t now) `iff` not (isBusinessHour s)

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
    let
        now = localTimeToUTC utc fridayNoon
        submissionDate = thursdayNoon
    in
    SubmitDateIsBeforeNow `elem` getValidationErrors submissionDate 8 now

negativeTurnAroundFails :: Bool
negativeTurnAroundFails = NegativeTurnaround `elem` getValidationErrors thursdayNoon (-10) (localTimeToUTC utc fridayNoon)

nonBusinessHourSubmissionFails :: Bool
nonBusinessHourSubmissionFails =
    SubmitDateIsNotBusinessHour `elem` getValidationErrors saturdayNoon 10 (localTimeToUTC utc saturdayNoon) &&
    SubmitDateIsNotBusinessHour `elem` getValidationErrors thursdayMidnight 10 (localTimeToUTC utc thursdayNoon)

validRequestDoesNotFail :: Bool
validRequestDoesNotFail = null $ getValidationErrors fridayNoon 8 (localTimeToUTC utc thursdayNoon)

nihgtsAreSkipped :: Bool
nihgtsAreSkipped = dueDate thursdayNoon 8 == fridayNoon

weekendsAreSkipped :: Bool
weekendsAreSkipped = dueDate thursdayNoon (3 * 8) == followingMondayNoon

{-----------------}
{-- TEST RUNNER --}
{-----------------}

return []
runAllTests = $quickCheckAll

main :: IO Bool
main = runAllTests
