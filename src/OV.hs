module OV
    ( findJourney
    , findValueInObject
    , findValueInArray
    , findValue
    , firstElem
    ) where

import Network.HTTP
import JSON
import ICS
import Data.Maybe ( mapMaybe)

findJourney :: IO ()
findJourney = get "http://api.9292.nl/0.1/journeys?before=1&sequence=1&byFerry=true&bySubway=true&byBus=true&byTram=true&byTrain=true&lang=nl-NL&from=station-amsterdam-centraal&dateTime=2020-10-21T1754&searchType=departure&interchangeTime=standard&after=5&to=station-eindhoven" >>= showJourney

showJourney :: String -> IO ()
--showJourney value = print value
showJourney value = 
    let result  = parseICS value
    in case result of
        Left problem -> putStrLn "Er is een fout opgetreden"
        Right ok     -> showFull (ok)

showFull :: ICS -> IO ()
showFull d = print d
--showFull value = case value of
-- JSObject o -> putStrLn (showObject o)

showObject :: [(String, JSON)] -> String
showObject object = let item = snd (object!!0)
 in case item of
  JSArray a -> showArray a
  _ -> ""

showArray :: [JSON] -> String
showArray array = let item = (array!!0)
 in case item of
  JSObject o -> showItem o
  _ -> ""

showItem :: [(String, JSON)] -> String
showItem object = let item = snd (object!!6)
 in case item of
  JSArray a -> showLegs a
  _ -> ""

showLegs :: [JSON] -> String
showLegs legs = unlines (map parseShowLeg legs)

parseShowLeg :: JSON -> String
parseShowLeg item = case item of
  JSObject o -> showLeg o
  _ -> ""

showLeg :: [(String, JSON)] -> String
showLeg leg = do
 let mode = findValue "mode" leg
 let name = case mode of JSObject object -> findValue "name" object
 let destination = findValue "destination" leg
 let stops = findValue "stops" leg
 let firstStop = case stops of JSArray array -> array!!0
 let departure = getDeparture firstStop

 "\n"++filterStringValue(name)++ " - " ++filterStringValue(destination) ++ "\n* Vertrek: "++departure

getDeparture :: JSON -> String
getDeparture firstStop = case firstStop of 
 JSObject object -> (filterStringValue(findValue "departure" object)++" - "++getObjectName(findValue "location" object))
 _ -> ""

getObjectName :: JSON -> String
getObjectName object = case object of 
 JSObject object -> (filterStringValue(findValue "name" object))
 _ -> ""

filterStringValue :: JSON -> String
filterStringValue value = case value of 
 JSString str -> str
 _ -> ""

findValue :: String -> [(String, JSON)] -> JSON
findValue query values = firstElem (findValueInArray query values)

firstElem :: [JSON] -> JSON
firstElem xs = case xs of
  [] -> JSString ""
  (x:_) -> x

findValueInArray :: String -> [(String, JSON)] -> [JSON]
findValueInArray query values = mapMaybe (findValueInObject query) values

findValueInObject :: String -> (String, JSON) -> Maybe JSON
findValueInObject query value = if
 (fst value) == query 
 then Just (snd value)
 else Nothing

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody
