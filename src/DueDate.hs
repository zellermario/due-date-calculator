module DueDate where

import Data.Time(LocalTime, UTCTime, getCurrentTime)

type Hours = Int
data ValidationError 
    = SubmitDateIsBeforeNow
    | SubmitDateIsNotBusinessHour
    | NegativeTurnaround
    deriving Eq

-- Stateful computation that gets current time from environment, validates inputs,
-- and forwards valid arguments to the pure dueDate function.
-- The result is either the calculated due date or a list of validation errors.
calculateDueDate :: LocalTime -> Hours -> IO (Either [ValidationError] LocalTime)
calculateDueDate submitDate turnAround = do
    now <- getCurrentTime
    let errors = getValidationErrors submitDate turnAround now
    return $ if null errors
        then Right (dueDate submitDate turnAround)
        else Left errors

getValidationErrors :: LocalTime -> Hours -> UTCTime -> [ValidationError]
getValidationErrors submitDate turnAround now = []

dueDate :: LocalTime -> Hours -> LocalTime
dueDate submitDate turnAround = submitDate

isBusinessDay :: LocalTime -> Bool
isBusinessDay dateTime = False

isBusinessHour :: LocalTime -> Bool
isBusinessHour dateTime = False
