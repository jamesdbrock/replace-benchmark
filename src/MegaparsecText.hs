{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Text.Megaparsec
import Replace.Megaparsec
import Data.Text.IO as T

main :: IO ()
main = T.getContents
    >>= streamEditT @() (chunk "x") (return . const "oo")
    >>= T.putStr
