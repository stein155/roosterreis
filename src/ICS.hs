module ICS (
    parseICS
  ) where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

data ICS = ICSNull
         | ICSValue String
         | ICSObject [(String, ICS)]
         deriving (Show, Eq)

parseICS :: String -> Either ParseError ICS
parseICS xs = parse value "json" xs

value :: Parser ICS
value = choice [calendar]

calendar :: Parser ICS
calendar = do
 string "BEGIN:VCALENDAR"
 decl <- many (noneOf "END:VCALENDAR")
 string "END:VCALENDAR"
 return (ICSValue decl)

--event :: Parser ICS
--event = do
-- string "BEGIN:VEVENT"
-- decl <- many (noneOf "END:VEVENT")
-- string "END:VEVENT"
-- return (ICSValue decl)