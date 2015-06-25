{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DigitalOcean.SSHKey (
  SSHKey(..),
  listAllKeys
                            ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class
import Data.Aeson ((.:), FromJSON(..), Value(..), decode)
import DigitalOcean.Base(Authentication, requestGet)

-- $setup
-- >>> import System.Environment
-- >>> import DigitalOcean.Base(Authentication(..))
-- >>> import Data.Maybe(isJust)

data SSHKey = SSHKey {
  id :: Integer,
  fingerprint  :: String,
  publicKey :: String,
  sshkeyName :: String
} deriving (Show, Read)

newtype SSHKeys = SSHKeys [SSHKey]

instance FromJSON SSHKeys where
  parseJSON (Object v) = SSHKeys <$> v .: "ssh_keys"
  parseJSON _ = mzero

instance FromJSON SSHKey where
  parseJSON (Object v) =
    SSHKey <$>
    (v .: "id") <*>
    (v .: "fingerprint") <*>
    (v .: "public_key") <*>
    (v .: "name")
  parseJSON _ = mzero

-- | List all Keys
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeKeys <- listAllKeys $ Authentication tkn
--     print $ show $ isJust maybeKeys
-- @
--
listAllKeys :: Authentication -> (MonadIO m) => m (Maybe [SSHKey])
listAllKeys a = liftM toList $ liftM decode (requestGet "account/keys" a)

toList :: Maybe SSHKeys -> Maybe [SSHKey]
toList = \sks ->  case sks of Just(SSHKeys l)  -> Just l
                              Nothing          -> Nothing
