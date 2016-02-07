module Gigasecond where
-- http://exercism.io/exercises/haskell/gigasecond
-- original question:
-- One billion seconds... Find out the exact second you were born (if you can).
-- Figure out when you will turn (or perhaps when you did turn?) one billion seconds old. Then go mark your calendar.
-- http://two-wrongs.com/haskell-time-library-tutorial

import Data.Time.Clock

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime (1000000000 :: NominalDiffTime)

