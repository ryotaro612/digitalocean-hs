{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DigitalOcean.Region(
  Region(..),
  regions
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

data Region = Region {
  regionName :: String,
  slug :: String,
  available :: Bool
} deriving (Show, Read)


-- | List all Regions
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeRegions <- regions $ Authentication tkn
--     print $ show $ isJust maybeRegions
-- @
--
regions :: Authentication -> (MonadIO m) => m (Maybe [Region])
regions a = liftM toList $ liftM decode (requestGet "regions" a)


newtype Regions = Regions [Region]

instance FromJSON Regions where
  parseJSON (Object v) = Regions <$> v.: "regions"
  parseJSON _          = mzero

instance FromJSON Region where
  parseJSON (Object v) =
    Region <$>
    (v .: "name") <*>
    (v .: "slug") <*>
    (v .: "available")
  parseJSON _ = mzero
    

toList :: Maybe Regions -> Maybe [Region]
toList = \rs ->  case rs of Just(Regions l) -> Just l
                            Nothing          -> Nothing
