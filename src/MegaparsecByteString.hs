{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Replace.Megaparsec
import Data.ByteString as BS

main :: IO ()
main = BS.getContents
        >>= streamEditT (chunk "x") (return . const "oo")
        >>= BS.putStr
