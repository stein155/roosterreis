module ISAS
    ( getActivitiesForTomorrow
    ) where

import Data.List (isInfixOf )
import Data.Maybe ( mapMaybe)
import Date
import Network

getActivitiesForTomorrow :: IO [String]
getActivitiesForTomorrow = do
 response <- performRequest "http://sascalendar.han.nl/getCalendar.aspx?type=ica&id=skmmb"
 date <- tomorrow
 return (output "20201026" response )
 --return (output (formatDateForIsas(date)) response)

output :: String -> String -> [String]
output date input = map formatDateForOv $ findTagsInLines (words input) date

findTagsInLines :: [String] -> String -> [String]
findTagsInLines lines date = mapMaybe (findTagInLine date) lines

findTagInLine :: String -> String -> Maybe String
findTagInLine date line = if 
 isInfixOf ("DTSTART:"++date) line 
 then Just (drop 8 line)
 else Nothing