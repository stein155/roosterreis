module Date 
 (formatDateForIsas
 ,formatDateForOv
 ,formatDateReadable
 ,today
 ,tomorrow)
 where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format ( defaultTimeLocale, formatTime)
import Data.Time

formatDateForIsas :: Day -> String
formatDateForIsas = formatTime defaultTimeLocale "%Y%m%d"

formatDateForOv :: String -> String
formatDateForOv val = (formatTime defaultTimeLocale "%Y-%m-%dT%H%M" (understandIsasDate val))

formatDateReadable :: String -> String
formatDateReadable val = (formatTime defaultTimeLocale "%d-%m-%Y %H:%M" (understandOvDate val))

understandIsasDate :: String -> UTCTime
-- Uur optellen bij iSAS tijd i.v.m. gebruik andere tijdzone
understandIsasDate value = addUTCTime 3600 (parseTimeOrError True defaultTimeLocale "%Y%m%dT%H%M%SZ" value)

understandOvDate :: String -> UTCTime
understandOvDate value = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H%M" (replace value ":" "")

today :: IO Day
today = fmap utctDay getCurrentTime

tomorrow :: IO Day
tomorrow = fmap (addDays 1) today

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)