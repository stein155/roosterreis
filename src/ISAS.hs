module ISAS
 (getActivitiesForTomorrow)
where

import Data.List (isInfixOf )
import Data.Maybe ( mapMaybe)
import Date
import Network

getActivitiesForTomorrow :: IO [String]
getActivitiesForTomorrow = do
 response <- performRequest "http://sascalendar.han.nl/getCalendar.aspx?type=ica&id=skmmb"
 date <- tomorrow
 return (findStartTimesInResponse (formatDateForIsas $ date) response)

findStartTimesInResponse :: String -> String -> [String]
findStartTimesInResponse date input = map formatDateForOv $ findStartTagInLines (words input) date

findStartTagInLines :: [String] -> String -> [String]
findStartTagInLines lines date = mapMaybe (findStartTagInLine date) lines

findStartTagInLine :: String -> String -> Maybe String
findStartTagInLine date line = if 
 isInfixOf ("DTSTART:"++date) line 
 then Just (drop 8 line)
 else Nothing