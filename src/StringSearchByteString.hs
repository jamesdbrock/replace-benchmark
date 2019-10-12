{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Search
import Data.ByteString as BS
import Data.ByteString.Lazy as BL

main :: IO ()
main = BL.putStr =<< replace "x" ("oo" :: BL.ByteString) <$> BS.getContents
