{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text.Lazy
import Replace.Attoparsec.Text.Lazy
import Data.Text.Lazy.IO as T

main :: IO ()
main = T.getContents
    >>= pure . streamEdit (string "x") (const "oo")
    >>= T.putStr
