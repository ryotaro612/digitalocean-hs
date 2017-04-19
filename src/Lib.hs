{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

--main :: IO ()
--main = httpLBS "http://example.com" >>= L8.putStrLn

someFunc :: IO ()
someFunc = putStrLn "someFunc"

request :: Request
request = "https://google.com"

-- >>= :: m a -> (a -> m b) -> m b
-- b :: IO(Response L8.ByteString)
--b = httpLBS request
aFun :: IO()
aFun = httpLBS "http://google.com" >>= L8.putStrLn



