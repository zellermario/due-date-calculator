{-# LANGUAGE TemplateHaskell #-}

module Tests where

import Data.Time (LocalTime, diffLocalTime)
import DueDate ( dueDate
               , getValidationErrors
               , isBusinessHour
               , isBusinessDay
               , Hours
               , ValidationError
               )
import Test.QuickCheck.All (quickCheckAll)
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

{-------------------------}
{-- CONCRETE TEST CASES --}
{-------------------------}



{-----------------}
{-- TEST RUNNER --}
{-----------------}

return []
runAllTests = $quickCheckAll

main :: IO Bool
main = runAllTests