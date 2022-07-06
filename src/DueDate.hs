module DueDate where

import Data.Time (LocalTime, getCurrentTime, UTCTime)

type Hours = Int
data ValidationError = SubmitDateIsBeforeNow | NegativeWorkingHours

-- Stateful computation that gets current time from environment, validates inputs,
-- and forwards valid arguments to the pure dueDate function.
-- The result is either the calculated due date or a list of validation errors.
calculateDueDate :: LocalTime -> Hours -> IO (Either [ValidationError] LocalTime)
calculateDueDate submitDate turnAround = do
    now <- getCurrentTime
    let errors = getValidationErrors
    return $ if null errors
        then Right (dueDate submitDate turnAround)
        else Left errors

getValidationErrors :: [ValidationError]
getValidationErrors = undefined

dueDate :: LocalTime -> Hours -> LocalTime
dueDate = undefined

isBusinessDay :: LocalTime -> Bool
isBusinessDay = undefined

isBusinessHour :: LocalTime -> Bool
isBusinessHour = undefined