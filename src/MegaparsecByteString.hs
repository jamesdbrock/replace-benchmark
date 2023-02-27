{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Text.Megaparsec
import Replace.Megaparsec
import Data.ByteString as BS

main :: IO ()
main = BS.getContents
        >>= pure . streamEdit @() (chunk "x") (const "oo")
        >>= BS.putStr
