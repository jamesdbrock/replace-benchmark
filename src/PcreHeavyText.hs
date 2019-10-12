{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.Regex.PCRE.Heavy
import Data.Text.IO as T
import Data.Text as T

main :: IO ()
main = T.putStr =<< gsub [re|x|] ("oo" :: T.Text) <$> T.getContents
