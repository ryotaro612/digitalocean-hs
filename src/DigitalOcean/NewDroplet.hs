{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module DigitalOcean.NewDroplet(
  newDroplet
) where

import Control.Applicative ((<$>))
import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class
import Data.Aeson ((.:),  FromJSON(..), Value(..), ToJSON(..), (.=), object, decode)

import DigitalOcean.Droplet(Droplet(..))
import DigitalOcean.Base

-- $setup
-- >>> {-# LANGUAGE OverloadedStrings #-}
-- >>> import Data.ByteString.Char8
-- >>> import System.Environment
-- >>> import DigitalOcean.Base(Authentication(..))
-- >>> import Data.Maybe(isJust)


instance ToJSON NewDropletRequest where
  toJSON v = object ["name" .= ndname v,
                     "region" .= region v,
                     "size" .= size v,
                     "image" .= image v,
                     "ssh_keys" .= ndsshKeys v,
                     "backups" .= backups v,
                     "ipv6" .= ipv6 v,
                     "user_data" .= userData v,
                     "private_networking" .= privateNetworking v
             ]

data NewDropletRequest  = NewDropletRequest {
  ndname :: String,
  region :: String, 
  size :: String,
  image :: Integer,
  ndsshKeys :: [String],
  backups :: Bool,
  ipv6 :: Bool,
  userData :: Maybe(String),
  privateNetworking :: Bool
} deriving (Show, Read)

newtype OuterDroplet = OuterDroplet Droplet deriving(Show)

instance FromJSON OuterDroplet where
  parseJSON (Object v) = OuterDroplet <$> v .: "droplet"
  parseJSON _          = mzero


-- | create new Droplet
--
-- @
-- do
--     let d = NewDropletRequest {
--         ndname="hoge",
--         region="nyc3",
--         size="512mb",
--         image=12241402,
--         ndsshKeys=[],
--         backups = False,
--         ipv6=True,
--         userData = Nothing,
--         privateNetworking=False
--     }
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeNewDroplet <- newDroplet ( Authentication tkn) d
-- @
--
newDroplet :: Authentication -> NewDropletRequest -> (MonadIO m) => m (Maybe Droplet)
newDroplet a d = liftM toDroplet $ liftM decode (requestPost "droplets" a  d)

toDroplet :: Maybe OuterDroplet -> Maybe Droplet
toDroplet = \od -> case od of Just(OuterDroplet d) -> Just d
                              Nothing              -> Nothing
