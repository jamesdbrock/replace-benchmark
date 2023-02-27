{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
import Replace.Attoparsec.ByteString
import Data.ByteString as BS

main :: IO ()
main = BS.getContents
        >>= pure . streamEdit (string "x") (const "oo")
        >>= BS.putStr
