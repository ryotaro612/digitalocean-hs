{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module DigitalOcean.Image(
  Image(..),
  images,
  listAllDistributionImages
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


data Image = Image {
  imgId :: Integer,
  imgName :: String,
  imgDistribution :: String,
  imgType :: String,
  imgSlug :: Maybe String,
  imgPublic :: Bool,
  imgRegions :: [String],
  imgMinDiskSize :: Integer,
  imgCreatedAt :: String
} deriving(Show, Read)

-- | List all Images
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeImages <- images $ Authentication tkn
--     print $ show $ isJust maybeImages
-- @
--
images :: Authentication -> (MonadIO m) => m (Maybe [Image])
images a = liftM toList $ liftM decode (requestGet "images?page=1&per_page=1" a)

-- | List all Distribution Images
--
-- @
-- do
--     tkn <- getEnv "DIGITAL_OCEAN_PERSONAL_ACCESS_TOKEN"
--     maybeImages <- listAllDistributionImages $ Authentication tkn
--     print $ show $ isJust maybeImages
-- @
--
listAllDistributionImages :: Authentication -> (MonadIO m) => m (Maybe [Image])
listAllDistributionImages a = liftM toList $ liftM decode (requestGet "images?page=1&per_page=100&type=distribution" a)

toList :: Maybe Images -> Maybe [Image]
toList = \ds ->  case ds of Just(Images l) -> Just l
                            Nothing -> Nothing


newtype Images = Images [Image]

instance FromJSON Images where
  parseJSON (Object v) = Images <$> v.: "images"
  parseJSON _          = mzero

instance FromJSON Image where
  parseJSON (Object v) =
    Image <$>
    (v .: "id") <*>
    (v .: "name") <*>
    (v .: "distribution") <*>
    (v .: "type") <*>
    (v .: "slug") <*>
    (v .: "public") <*>
    (v .: "regions") <*>
    (v .: "min_disk_size") <*>
    (v .: "created_at")
  parseJSON _ = mzero
