{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module DigitalOcean.Size(
  Size(..),
  sizes
                        ) where

import Control.Applicative ()
import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class
import Data.Aeson ((.:), FromJSON(..), Value(..), decode)
import DigitalOcean.Base(Authentication,  requestGet)

-- $setup
-- >>> import System.Environment
-- >>> import DigitalOcean.Base(Authentication(..))
-- >>> import Data.Maybe(isJust)

data Size = Size {
  sizSlug :: String,
  sizAvailable  :: Bool,
  sizTransfer :: Integer,
  sizPriceMonthly :: Float,
  sizHourly :: Float,
  sizMemory :: Integer,
  sizVcpus :: Integer,
  sizDisk :: Integer
} deriving(Show, Read)

newtype Sizes = Sizes [Size]

instance FromJSON Sizes where
  parseJSON (Object v) = Sizes <$> v.: "sizes"
  parseJSON _          = mzero

-- | List all Sizes
-- 
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeSizes <- sizes $ Authentication tkn
--     print $ show $ isJust maybeSizes
-- @
-- 
sizes :: Authentication -> (MonadIO m) => m (Maybe [Size])
sizes  a = liftM toList $ liftM decode (requestGet "sizes" a)

instance FromJSON Size where
  parseJSON (Object v) =
    Size <$>
    (v .: "slug") <*>
    (v .: "available") <*>
    (v .: "transfer") <*>
    (v .: "price_monthly") <*>
    (v .: "price_hourly") <*>
    (v .: "memory") <*>
    (v .: "vcpus") <*>
    (v .: "disk")
  parseJSON _ = mzero
   
toList :: Maybe Sizes -> Maybe [Size]
toList = \ss ->  case ss of Just(Sizes l) -> Just l
                            Nothing          -> Nothing
