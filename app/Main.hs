import OV
import ISAS
import Date

ovFrom = "station-zevenaar"
ovTo = "station-arnhem-presikhaaf"

main :: IO ()
main = do
 activities <- getActivitiesForTomorrow
 if((length $ activities) > 0)
  then do showActivityRoute (activities!!0)
  else do showError

showActivityRoute :: String -> IO ()
showActivityRoute activity = do
 putStrLn $ "Morgen begint de eerste les: "++ (formatDateReadable activity)

 putStrLn $ "\nDe volgende OV-reis wordt geadviseerd:"
 findJourney ovFrom ovTo activity

showError :: IO ()
showError = putStrLn "Je hebt morgen geen les!"