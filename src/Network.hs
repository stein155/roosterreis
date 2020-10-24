module Network
    ( performRequest
    ) where

import Network.HTTP

performRequest :: String -> IO String
performRequest url = simpleHTTP (getRequest url) >>= getResponseBody