{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.DocTest

main :: IO ()
main = do
  doctest ["-isrc", "src/DigitalOcean.hs"]
{-
  doctest [
             "-isrc",
             "src/DigitalOcean/Droplet.hs",
             "src/DigitalOcean/Account.hs",
             "src/DigitalOcean/SSHKey.hs",
             "src/DigitalOcean/Image.hs",
             "src/DigitalOcean/Region.hs",
             "src/DigitalOcean/Size.hs"
-}
--             "src/DigitalOcean/NewDroplet.hs"
--             ]
