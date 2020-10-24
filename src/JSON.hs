module JSON
 (JSON(..)
 ,parseJSON)
 where

import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Text.Parsec
import Text.Parsec.String

data JSON = JSONNull
         | JSONBool Bool
         | JSONObject [(String, JSON)]
         | JSONArray [JSON]
         | JSONString String
         | JSONValue String
         | JSONNumber Float
         deriving (Show, Eq)

parseJSON :: String -> Either ParseError JSON
parseJSON xs = parse json "json" xs

json :: Parser JSON
json = ws *> value

value :: Parser JSON
value = lexeme $ (jsonNull <|> jsonBool <|> jsonString <|> jsonNumber <|> jsonObject <|> jsonArray)

jsonNull :: Parser JSON
jsonNull = do
 string "null"
 return (JSONNull)

jsonBool :: Parser JSON
jsonBool = jsonTrue <|> jsonFalse
 
jsonTrue :: Parser JSON
jsonTrue = do
 string "true"
 return (JSONBool True)

jsonFalse :: Parser JSON
jsonFalse = do
 string "false"
 return (JSONBool False)

jsonObject :: Parser JSON
jsonObject = JSONObject <$> ((lexeme $ char '{') *> 
 (sepBy objectEntry (lexeme $ char ','))
 <* (char '}'))

objectEntry :: Parser (String, JSON)
objectEntry = do 
 key <- stringLiteral
 lexeme $ char ':'
 value <- value
 return (key, value)

jsonString :: Parser JSON
jsonString = do
 string "\""
 value <- many (noneOf "\"")
 string "\""
 return (JSONString value)

stringLiteral :: Parser String
stringLiteral = do
 string "\""
 value <- many (noneOf "\"")
 string "\""
 return (value)

jsonArray :: Parser JSON
jsonArray = JSONArray <$> ((lexeme $ char '[') *>
 (sepBy value (lexeme $ char ','))
 <* (lexeme $ char ']'))

jsonNumber :: Parser JSON
jsonNumber = do
 value <- many1 (oneOf "0123456789.")
 return $ JSONNumber (read value::Float)

lexeme p = p <* ws

ws :: Parser String
ws = many (oneOf " \r\n\t")