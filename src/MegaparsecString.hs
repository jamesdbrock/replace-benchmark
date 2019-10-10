import Text.Megaparsec
import Replace.Megaparsec

main :: IO ()
main = getContents
    >>= streamEditT (chunk "x") (return . const "oo")
    >>= putStr
