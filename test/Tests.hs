import ArbitraryTime
import BusinessWeek
import Data.Time
    ( dayOfWeek,
      midnight,
      midday,
      fromGregorian,
      diffLocalTime,
      LocalTime(..),
      TimeOfDay(TimeOfDay)
    )
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

conversionRoundTrip :: LocalTime -> Bool
conversionRoundTrip date =
    let
        inputDow = dayOfWeek $ localDay date
        inputTod = localTimeOfDay date
        (outputDow, outputTod) = fromTimeOfBusinessWeek . toTimeOfBusinessWeek $ date
    in
        isBusinessHour date `implies` ((inputDow == outputDow) && (inputTod `minutePrecisionEqualsTod` outputTod))

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

exampleSubmissionTimeA :: LocalTime
exampleSubmissionTimeA = LocalTime {
    localDay = fromGregorian 2022 7 8,
    localTimeOfDay = TimeOfDay 13 48 00
}

exampleTurnaroundA = 92

exampleExpectedDueDateA :: LocalTime
exampleExpectedDueDateA = LocalTime {
    localDay = fromGregorian 2022 7 26,
    localTimeOfDay = TimeOfDay 9 48 00
}

exampleSubmissionTimeB :: LocalTime
exampleSubmissionTimeB = LocalTime {
    localDay = fromGregorian 2022 7 7, -- Thursday
    localTimeOfDay = TimeOfDay 10 00 00
}

exampleTurnaroundB = 7

exampleExpectedDueDateB :: LocalTime
exampleExpectedDueDateB = LocalTime {
    localDay = fromGregorian 2022 7 8, -- Friday
    localTimeOfDay = TimeOfDay 9 00 00
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

weekEndIsNotBusinessHour :: Bool
weekEndIsNotBusinessHour = not $ isBusinessHour saturdayNoon

nightIsNotBusinessHour :: Bool
nightIsNotBusinessHour = not $ isBusinessHour thursdayMidnight

thursDayNoonIsBusinessHour :: Bool
thursDayNoonIsBusinessHour = isBusinessHour thursdayNoon

exampleA :: Bool
exampleA = dueDate exampleSubmissionTimeA exampleTurnaroundA `minutePrecisionEquals` exampleExpectedDueDateA

exampleB :: Bool
exampleB = dueDate exampleSubmissionTimeB exampleTurnaroundB `minutePrecisionEquals` exampleExpectedDueDateB

{-----------------}
{-- TEST RUNNER --}
{-----------------}

-- The template extension didn't want to work so I had to list all properties

main :: IO ()
main = do
    quickCheck zeroIsIdentityForTurnaround
    quickCheck distributiveInTurnaround
    quickCheck commutativeInTurnAround
    quickCheck onlyNegativeTurnaroundFails
    quickCheck onlyNonBusinessHourSubmissionFails
    quickCheck onlyPastSubmissionDateFails
    quickCheck eightBusinessHoursCorrespondToFullDay
    quickCheck fiveBusinessDaysCorrespondToFullWeek
    quickCheck conversionRoundTrip
    quickCheck pastSubmissionDateFails
    quickCheck negativeTurnAroundFails
    quickCheck nonBusinessHourSubmissionFails
    quickCheck validRequestDoesNotFail
    quickCheck nightsAreSkipped
    quickCheck weekendsAreSkipped
    quickCheck weekEndIsNotBusinessHour
    quickCheck nightIsNotBusinessHour
    quickCheck thursDayNoonIsBusinessHour
    quickCheck exampleA
    quickCheck exampleB