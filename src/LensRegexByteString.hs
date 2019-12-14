{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens.Regex.ByteString
import Data.ByteString as BS
import Control.Lens.Setter

main :: IO ()
main = BS.putStr =<< ([regex|x|] . match %~ const "oo") <$> BS.getContents
