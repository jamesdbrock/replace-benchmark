{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import Replace.Attoparsec.Text
import Data.Text.IO as T

main :: IO ()
main = T.getContents
    >>= pure . streamEdit (string "x") (const "oo")
    >>= T.putStr
