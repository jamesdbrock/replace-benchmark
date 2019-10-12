{-# LANGUAGE OverloadedStrings #-}

import Text.Regex.Applicative

main :: IO ()
main = putStr =<< replace ("oo" <$ "x") <$> getContents
