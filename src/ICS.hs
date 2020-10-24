module ICS (
    ICS(..)
    ,parseICS
  ) where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

data ICS = ICSNull
         | ICSBool Bool
         | ICSObject [(String, ICS)]
         | ICSArray [ICS]
         | ICSString String
         | ICSValue String
         | ICSNumber Float
         deriving (Show, Eq)

parseICS :: String -> Either ParseError ICS
parseICS xs = parse ics "json" xs

ics :: Parser ICS
ics = ws *> value

value :: Parser ICS
value = lexeme $ (icsNumber <|> icsNull <|> icsBool <|> icsString <|> icsObject <|> icsArray)

icsNull :: Parser ICS
icsNull = do
 string "null"
 return (ICSNull)

icsBool :: Parser ICS
icsBool = icsTrue <|> icsFalse
 
icsTrue :: Parser ICS
icsTrue = do
 string "true"
 return (ICSBool True)

icsFalse :: Parser ICS
icsFalse = do
 string "false"
 return (ICSBool False)

icsObject :: Parser ICS
icsObject = ICSObject <$> ((lexeme $ char '{') *> 
 (sepBy objectEntry (lexeme $ char ','))
 <* (char '}'))

objectEntry :: Parser (String, ICS)
objectEntry = do 
 key <- stringLiteral
 lexeme $ char ':'
 value <- value
 return (key, value)

icsString :: Parser ICS
icsString = do
 string "\""
 value <- many (noneOf "\"")
 string "\""
 return (ICSString value)

stringLiteral :: Parser String
stringLiteral = do
 string "\""
 value <- many (noneOf "\"")
 string "\""
 return (value)

icsArray :: Parser ICS
icsArray = ICSArray <$> ((lexeme $ char '[') *>
 (sepBy value (lexeme $ char ','))
 <* (lexeme $ char ']'))

icsNumber :: Parser ICS
icsNumber = do
 value <- many1 (oneOf "0123456789.")
 return $ ICSNumber (read value::Float)

lexeme p = p <* ws

ws :: Parser String
ws = many (oneOf " \r\n\t")