{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString
import Replace.Attoparsec.ByteString
import Data.ByteString as BS

main :: IO ()
main = BS.getContents
        >>= streamEditT (string "fnord") (return . const "bar")
        >>= BS.putStr
