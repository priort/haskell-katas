{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AlphabetCipher(alphabetCipher, alphabetDecipher) where
import Data.Char
import Data.Maybe

newtype AlphaChar = AlphaChar Char deriving (Ord, Enum, Bounded, Eq, Show)

toAlphaChar :: Char -> Maybe AlphaChar
toAlphaChar c = if c `elem` ['A' .. 'Z'] || c `elem` ['a' .. 'z'] then Just (AlphaChar c) else Nothing 

fromAlphaChar :: AlphaChar -> Char
fromAlphaChar (AlphaChar c) = c

isAlphaChar :: Char -> Bool 
isAlphaChar = isJust . toAlphaChar

type AlphaCharRotater = AlphaChar -> AlphaChar -> Char

alphabetCipher :: String -> String -> Maybe String
alphabetCipher message = transformMessage rotateRight message . cycle 

alphabetDecipher :: String -> String -> Maybe String
alphabetDecipher message = transformMessage rotateLeft message . cycle 
 
transformMessage :: AlphaCharRotater -> String -> String -> Maybe String 
transformMessage rotateFunc (m : ms) (k : ks) =
  if not (isAlphaChar m) then do 
    restMessage <- transformMessage rotateFunc ms (k : ks)
    return (m : restMessage) 
  else do
    keyChar <- toAlphaChar k
    messageChar <- toAlphaChar m
    restMessage <- transformMessage rotateFunc ms ks
    return (rotateFunc keyChar messageChar : restMessage) 
transformMessage _ _ _ = Just ""

rotateRight :: AlphaChar -> AlphaChar -> Char
rotateRight = rotate (+)

rotateLeft :: AlphaChar -> AlphaChar -> Char 
rotateLeft = rotate (-)

rotate :: (Int -> Int -> Int) -> AlphaChar -> AlphaChar -> Char
rotate directionFunc (AlphaChar keywordChar) (AlphaChar c) = 
  chr (ordA c + mod ((ord c - ordA c) `directionFunc` numShiftPlaces keywordChar) alphabetLength) 

numShiftPlaces :: Char -> Int 
numShiftPlaces keywordC = (ord . toLower $ keywordC) - ord 'a'

ordA :: Char -> Int
ordA c
  | isUpper c = ord 'A'
  | otherwise = ord 'a'

alphabetLength :: Int
alphabetLength = 26
