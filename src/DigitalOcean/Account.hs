{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DigitalOcean.Account (
  Account(..),
  account
                            ) where

import Control.Applicative ()
import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class
import Data.Aeson ((.:), FromJSON(..), Value(..), decode)

import DigitalOcean.Base(Authentication, requestGet)

-- $setup
-- >>> import System.Environment
-- >>> import DigitalOcean.Base(Authentication(..))
-- >>> import Data.Maybe(isJust)

data Account = Account {
  dropletLimit :: Integer,
  email :: String,
  uuid :: String,
  emailVerified :: Bool
} deriving (Show, Read)

newtype RawAccount = RawAccount Account

instance FromJSON RawAccount where
  parseJSON (Object v) = RawAccount <$> v .: "account"
  parseJSON _ = mzero

instance FromJSON Account where
  parseJSON (Object v) =
    Account <$>
    (v .: "droplet_limit") <*>
    (v .: "email") <*>
    (v .: "uuid") <*>
    (v .: "email_verified")
  parseJSON _ = mzero

-- | Get User Information
-- Example:
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     mayBeAcnt <- account $ Authentication tkn
--     print $ show $ isJust mayBeAcnt
-- @
--
account :: Authentication -> (MonadIO m) => m (Maybe Account)
account a = liftM toAccount $ liftM decode (requestGet "account" a)

toAccount :: Maybe RawAccount -> Maybe Account
toAccount = \ra ->  case ra of Just(RawAccount a) -> Just a
                               Nothing            -> Nothing
