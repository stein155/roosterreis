module Lib
    ( showActivitiesForDate
    , getActivitiesForToday
    , get
    , output
    , findTagInLine
    , findTagsInLines
    , formatDate
    ) where

import Network.HTTP
import Data.List.Split (splitOn)
import Data.List (isInfixOf )
import Data.Maybe ( mapMaybe)
import Data.Time.Clock
import Data.Time.Format ( defaultTimeLocale, formatTime)

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

getActivitiesForToday :: IO ()
getActivitiesForToday = do
 t <- getCurrentTime
 showActivitiesForDate (formatDate t)

showActivitiesForDate :: String -> IO ()
showActivitiesForDate date = do
 get "http://sascalendar.han.nl/getCalendar.aspx?type=ica&id=skmmb" >>= output date

output :: String -> String -> IO ()
output date input = print (findTagsInLines (words input) date)

findTagsInLines :: [String] -> String -> [String]
findTagsInLines lines date = mapMaybe (findTagInLine date) lines

findTagInLine :: String -> String -> Maybe String
findTagInLine date line = if 
 isInfixOf ("DTSTART:"++date) line 
 then Just (drop 8 line)
 else Nothing

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y%m%d"