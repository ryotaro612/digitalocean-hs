{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DigitalOcean.Droplet(
  Droplet(..),
  droplets
                           ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class
import Data.Aeson ((.:),  FromJSON(..), Value(..), decode)
import DigitalOcean.Base

-- $setup
-- >>> import System.Environment
-- >>> import DigitalOcean.Base(Authentication(..))
-- >>> import Data.Maybe(isJust)

data Droplet = Droplet {
  did :: Integer,
  dname :: String,
  dmemory :: Integer,
  dvcpus :: Integer,
  disk :: Integer,
  dlocked :: Bool,
  createdAtv2 :: String,
  dstatus :: String,
  backupIds :: [Integer]
} deriving (Show, Read)

newtype Droplets = Droplets [Droplet]

instance FromJSON Droplets where
  parseJSON (Object v) = Droplets <$> v.: "droplets"
  parseJSON _          = mzero
  

instance FromJSON Droplet where
  parseJSON (Object v) =
    Droplet <$>
    (v .: "id") <*>
    (v .: "name") <*>
    (v .: "memory") <*>
    (v .: "vcpus") <*>
    (v .: "disk") <*>
    (v .: "locked") <*>
    (v .: "created_at") <*>
    (v .: "status") <*>
    (v .: "backup_ids")
  parseJSON _ = mzero


-- | List all Droplets
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeDroplets <- droplets $ Authentication tkn
--     print $ show $ isJust maybeDroplets
-- @
-- 
droplets :: Authentication -> (MonadIO m) => m (Maybe [Droplet])
droplets a = liftM toList $ liftM decode (requestGet "droplets?page=1&per_page=100" a)

toList :: Maybe Droplets -> Maybe [Droplet]
toList = \ds ->  case ds of Just(Droplets l) -> Just l
                            Nothing          -> Nothing
