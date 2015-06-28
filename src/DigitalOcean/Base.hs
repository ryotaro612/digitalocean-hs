{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DigitalOcean.Base (
  Authentication(..),
  requestGet,
  requestPost
                         ) where

import Control.Monad.IO.Class
import Data.Aeson ( ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy(ByteString)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Data.ByteString.Char8(pack)
import Network.HTTP.Types.Method(methodPost, methodGet, Method)

-- Authentication "api token"
data Authentication = Authentication {
  token    :: String
} deriving (Show)


url :: String
url = "https://api.digitalocean.com/v2"

simpleHttp' :: MonadIO m => String -> Authentication -> Method -> Maybe(ByteString) -> m BS.ByteString
simpleHttp' url' auth reqMethod reqBody = liftIO $ withManager $ \man -> do  
  initReq <- liftIO $ parseUrl url'
  let req = initReq {
        method = reqMethod,
        requestHeaders = [(hContentType, Data.ByteString.Char8.pack "application/json"),
                         (hAuthorization,  Data.ByteString.Char8.pack $ "Bearer " ++ (token auth))],
        responseTimeout = Just 10000000, -- 10 seconds
        requestBody = case reqBody of Just b  -> RequestBodyLBS b
                                      Nothing -> requestBody initReq
  }
  responseBody <$> httpLbs req man


--dumpP :: (ToJSON j) =>  String -> Authentication -> j -> (MonadIO m) => m (BS.ByteString)
--dumpP   suburl auth body = simpleHttp' (url ++ "/" ++ suburl) auth methodPost $ Just (encode body)

--dumpG ::   String -> Authentication -> (MonadIO m) => m (BS.ByteString)
--dumpG   suburl auth = simpleHttp' (url ++ "/" ++ suburl) auth methodGet  Nothing

requestGet ::  String -> Authentication -> (MonadIO m) => m BS.ByteString
requestGet suburl auth = simpleHttp' (url ++ "/" ++ suburl)  auth methodGet Nothing


requestPost :: (ToJSON b) =>  String -> Authentication -> b -> (MonadIO m) => m BS.ByteString
requestPost suburl auth  body = simpleHttp' (url ++ "/" ++ suburl)  auth methodPost $ Just (encode body)
