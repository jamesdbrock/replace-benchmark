{-# LANGUAGE OverloadedStrings #-}

import Data.Text (replace)
import Data.Text.IO as T

main :: IO ()
main = T.getContents
    >>= pure . replace "x" "oo"
    >>= T.putStr
