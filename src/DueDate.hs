module DueDate where

import BusinessWeek
import Data.Time
    ( utcToLocalTime,
      LocalTime(LocalTime),
      getCurrentTime,
      addDays,
      dayOfWeek,
      getTimeZone 
    )

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
    timezone <- getTimeZone now
    let localNow = utcToLocalTime timezone now
    let errors = getValidationErrors submitDate turnAround localNow
    return $ if null errors
        then Right (dueDate submitDate turnAround)
        else Left errors

getValidationErrors :: LocalTime -> Hours -> LocalTime -> [ValidationError]
getValidationErrors submitDate turnAround now =
    [SubmitDateIsBeforeNow | submitDate < now ] ++
    [SubmitDateIsNotBusinessHour | not . isBusinessHour $ submitDate] ++
    [NegativeTurnaround | turnAround < 0]

dueDate :: LocalTime -> Hours -> LocalTime
dueDate submitDate@(LocalTime submitDay _) turnAround =
    let
        submitBusinessTime = toTimeOfBusinessWeek submitDate
        (deadlineBusinessTime, weekDifference) = submitBusinessTime `plus` turnAround
        (deadLineDayOfWeek, deadLineTimeOfDay) = fromTimeOfBusinessWeek deadlineBusinessTime
        weekDayDifference = fromEnum deadLineDayOfWeek - fromEnum (dayOfWeek submitDay)
        dayDifference = toInteger $ weekDifference * 7 + weekDayDifference
        deadLineDay = addDays dayDifference submitDay
    in
        LocalTime deadLineDay deadLineTimeOfDay
