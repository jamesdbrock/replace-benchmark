import Text.Megaparsec
import Replace.Megaparsec

main :: IO ()
main = getContents
    >>= streamEditT (chunk "fnord") (return . const "bar")
    >>= putStr
