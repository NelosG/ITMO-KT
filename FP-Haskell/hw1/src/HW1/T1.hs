module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import GHC.Natural (Natural)

-- | Datatype that represents days of week
data Day =
  Monday       -- ^ Monday
  | Tuesday    -- ^ Tuesday
  | Wednesday  -- ^ Wednesday
  | Thursday   -- ^ Thursday
  | Friday     -- ^ Friday
  | Saturday   -- ^ Saturday
  | Sunday     -- ^ Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay day = case day of
  Monday    -> Tuesday
  Tuesday   -> Wednesday
  Wednesday -> Thursday
  Thursday  -> Friday
  Friday    -> Saturday
  Saturday  -> Sunday
  Sunday    -> Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays 0 day = day
afterDays a day = afterDays (a - 1) $ nextDay day

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
daysToParty = daysToPartyImpl 0

-- | Support function.
daysToPartyImpl :: Natural -> Day -> Natural
daysToPartyImpl a Friday = a
daysToPartyImpl a day    = daysToPartyImpl (a + 1) $ nextDay day
