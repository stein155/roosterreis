module Lib
    ( showActivitiesForDate
    , getActivitiesForTomorrow
    , output
    , findTagInLine
    , findTagsInLines
    , formatDate
    , today
    , tomorrow
    ) where

import Network.HTTP
import Data.List.Split (splitOn)
import Data.List (isInfixOf )
import Data.Maybe ( mapMaybe)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format ( defaultTimeLocale, formatTime)

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getActivitiesForTomorrow :: IO ()
getActivitiesForTomorrow = do
 t <- tomorrow
 showActivitiesForDate (formatDate t)

showActivitiesForDate :: String -> IO ()
showActivitiesForDate date = get "http://sascalendar.han.nl/getCalendar.aspx?type=ica&id=skmmb" >>= output date

output :: String -> String -> IO ()
output date input = print (findTagsInLines (words input) date)

findTagsInLines :: [String] -> String -> [String]
findTagsInLines lines date = mapMaybe (findTagInLine date) lines

findTagInLine :: String -> String -> Maybe String
findTagInLine date line = if 
 isInfixOf ("DTSTART:"++date) line 
 then Just (drop 8 line)
 else Nothing

formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale "%Y%m%d"

today :: IO Day
today = fmap utctDay getCurrentTime

tomorrow :: IO Day
tomorrow = fmap (addDays 1) today