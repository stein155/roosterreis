module OV ( findJourney ) where

import Network.HTTP
import ICS

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

findJourney :: IO ()
findJourney = get "http://api.9292.nl/0.1/journeys?before=1&sequence=1&byFerry=true&bySubway=true&byBus=true&byTram=true&byTrain=true&lang=nl-NL&from=station-zevenaar&dateTime=2020-10-21T1754&searchType=departure&interchangeTime=standard&after=5&to=station-arnhem-presikhaaf" >>= showJourney

showJourney :: String -> IO ()
showJourney value = 
 let result = parseICS value
 in case result of
  Right (ICSObject document) -> do
   case lookup "journeys" document of
    Just (ICSArray journeys) -> do
     case journeys!!0 of
      (ICSObject journey) -> do
       case lookup "legs" journey of
        Just (ICSArray legs) -> do
         printLegs $ legs

printLegs :: [ICS] -> IO ()
printLegs legs = mapM_ printLeg legs

printLeg :: ICS -> IO ()
printLeg leg = case leg of
 (ICSObject object) -> do
  case lookup "mode" object of
   Just (ICSObject mode) -> do
    case lookup "name" mode of
     Just (ICSString name) -> do
      putStr $ name     

  putStr " - "

  case lookup "destination" object of
   Just (ICSString destination) -> do
    putStr $ destination

  putStr "\n"

  putStr "Opstappen: "

  case lookup "stops" object of
   Just (ICSArray stops) -> do
    case stops!!0 of
     (ICSObject firstStop) -> do
      case lookup "departure" firstStop of
       Just (ICSString departure) -> do
        putStr $ departure

      putStr " - "

      case lookup "location" firstStop of
       Just (ICSObject location) -> do
        case lookup "stationType" location of
         Just (ICSString stationType) -> do
          putStr $ stationType
        
        putStr " "

        case lookup "name" location of
         Just (ICSString name) -> do
          putStr $ name

      putStr "\n\n"