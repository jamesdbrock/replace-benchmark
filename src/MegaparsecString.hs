{-# LANGUAGE TypeApplications #-}

import Text.Megaparsec
import Replace.Megaparsec

main :: IO ()
main = getContents
    >>= pure . streamEdit @() (chunk "x") (const "oo")
    >>= putStr
