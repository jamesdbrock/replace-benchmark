{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens.Regex.Text
import Data.Text.IO as T
import Data.Text as T
import Control.Lens.Setter

main :: IO ()
main = T.putStr =<< ([regex|x|] . match %~ const "oo") <$> T.getContents
