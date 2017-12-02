import           Data.Char
import           Data.List

-- symbol list
symbols :: [Char]
symbols = ['*', '`', '~', '!', '@', '#', '$', '%', '^', '&']

-- converts digit to symbol
digitToSymbol :: Char -> Char
digitToSymbol digit = symbols !! (digitToInt digit)

-- converts symbol to digit
symbolToDigit :: Char -> Char
symbolToDigit symbol = intToDigit (head (elemIndices symbol symbols))

-- adds two lower case chars, and wrap if result is exceeding 'z'
addChars :: Char -> Char -> Char
addChars char1 char2
    | char <= ord 'z' = chr char
    | otherwise = chr (char - steps * 26)
    where
        char = ord char1 + ord char2
        steps = ceiling (fromIntegral (char - ord 'z') / 26)

-- subs two lower case chars, and unwrap the result
subChars :: Char -> Char -> Char
subChars char1 char2
    = chr (char + steps * 26)
    where
        char = ord char1 - ord char2
        steps = floor (fromIntegral (ord 'z' - char) / 26)

-- encrypts text using key starting from offset using symmetric algorithm
encrypt :: String -> String -> Int -> String
encrypt [] key offset = []
encrypt (char:text) key offset
    | isUpper char = char:(encrypt text key offset)
    | isDigit char = (digitToSymbol char):(encrypt text key offset)
    | isLower char = (addChars char (key !! offset)):(encrypt text key (mod (offset + 1) (length key)))
    | otherwise = error "Cannot encrypt this text."

-- decrypts text using key starting from offset using symmetric algorithm
decrypt :: String -> String -> Int -> String
decrypt [] key offset = []
decrypt (char:text) key offset
    | isUpper char      = (char):(decrypt text key offset)
    | elem char symbols = (symbolToDigit char):(decrypt text key offset)
    | isLower char      = (subChars char (key !! offset)):(decrypt text key (mod (offset + 1) (length key)))
    | otherwise = error "Cannot decrypt this text."

-- utility for encryption
cipher :: String -> String -> String
cipher text key = encrypt text key 0

-- utility for decryption
decipher :: String -> String -> String
decipher text key = decrypt text key 0

-- takes input and shows output
main :: IO ()
main = do
    putStr "Enter text: "
    text <- getLine
    putStr "Enter key: "
    key <- getLine
    let encrypted = cipher text key
    putStrLn ("Encrypted is: " ++ encrypted)
    let decrypted = decipher encrypted key
    putStrLn ("Decrypted is: " ++ decrypted)
