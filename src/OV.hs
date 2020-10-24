module OV ( findJourney ) where

import Network
import JSON
import Date

findJourney :: String -> String -> String -> IO ()
findJourney from to dateTime = performRequest ("http://api.9292.nl/0.1/journeys?before=1&sequence=1&byFerry=true&bySubway=true&byBus=true&byTram=true&byTrain=true&lang=nl-NL&from="++from++"&dateTime="++dateTime++"&searchType=arrival&interchangeTime=standard&after=5&to="++to) >>= printJourney

printJourney :: String -> IO ()
printJourney value = 
 let result = parseJSON value
 in case result of
  Right (JSONObject document) -> do
   case lookup "journeys" document of
    Just (JSONArray journeys) -> do
     case journeys!!0 of
      (JSONObject journey) -> do
       case lookup "legs" journey of
        Just (JSONArray legs) -> do
         printLegs $ legs

printLegs :: [JSON] -> IO ()
printLegs legs = mapM_ printLeg legs

printLeg :: JSON -> IO ()
printLeg leg = case leg of
 (JSONObject object) -> do
  case lookup "mode" object of
   Just (JSONObject mode) -> do
    case lookup "name" mode of
     Just (JSONString name) -> do
      putStr $ name     

  putStr " - "

  case lookup "destination" object of
   Just (JSONString destination) -> do
    putStr $ destination

  putStr "\n"

  putStr "- Opstappen: "

  case lookup "stops" object of
   Just (JSONArray stops) -> do
    case stops!!0 of
     (JSONObject firstStop) -> do
      case lookup "departure" firstStop of
       Just (JSONString departure) -> do
        putStr $ formatDateReadable $ departure

      putStr " - "

      case lookup "location" firstStop of
       Just (JSONObject location) -> do
        case lookup "stationType" location of
         Just (JSONString stationType) -> do
          putStr $ stationType
        
        putStr " "

        case lookup "name" location of
         Just (JSONString name) -> do
          putStr $ name

      putStr "\n\n"