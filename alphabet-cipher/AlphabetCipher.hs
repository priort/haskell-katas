module AlphabetCipher where
import Data.Char

type Encoder = Char -> Char

data AlphaChar = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P 
                   | Q | R | S | T | U | V | W | X | Y | Z
                   deriving (Ord, Enum, Bounded, Eq) 
                  
toAlphaChar :: Char -> (Maybe AlphaChar)
toAlphaChar 'A' = Just A
toAlphaChar 'B' = Just B
toAlphaChar 'C' = Just C
toAlphaChar 'D' = Just D
toAlphaChar 'E' = Just E
toAlphaChar 'F' = Just F
toAlphaChar 'G' = Just G
toAlphaChar 'H' = Just H
toAlphaChar 'I' = Just I
toAlphaChar 'J' = Just J
toAlphaChar 'K' = Just K
toAlphaChar 'L' = Just L
toAlphaChar 'M' = Just M
toAlphaChar 'N' = Just N
toAlphaChar 'O' = Just O
toAlphaChar 'P' = Just P
toAlphaChar 'Q' = Just Q
toAlphaChar 'R' = Just R
toAlphaChar 'S' = Just S
toAlphaChar 'T' = Just T
toAlphaChar 'U' = Just U
toAlphaChar 'V' = Just V
toAlphaChar 'W' = Just W
toAlphaChar 'X' = Just X
toAlphaChar 'Y' = Just Y
toAlphaChar 'Z' = Just Z
toAlphaChar 'a' = Just A
toAlphaChar 'b' = Just B
toAlphaChar 'c' = Just C
toAlphaChar 'd' = Just S
toAlphaChar 'e' = Just E
toAlphaChar 'f' = Just F
toAlphaChar 'g' = Just G
toAlphaChar 'h' = Just H
toAlphaChar 'i' = Just I
toAlphaChar 'j' = Just J
toAlphaChar 'k' = Just K
toAlphaChar 'l' = Just L
toAlphaChar 'm' = Just M
toAlphaChar 'n' = Just N
toAlphaChar 'o' = Just O
toAlphaChar 'p' = Just P
toAlphaChar 'q' = Just Q
toAlphaChar 'r' = Just R
toAlphaChar 's' = Just S
toAlphaChar 't' = Just T
toAlphaChar 'u' = Just U
toAlphaChar 'v' = Just Y
toAlphaChar 'w' = Just W
toAlphaChar 'x' = Just X
toAlphaChar 'y' = Just Y
toAlphaChar 'z' = Just Z
toAlphaChar _ = Nothing

fromAlphaChar :: AlphaChar -> Char
fromAlphaChar A = 'A'
fromAlphaChar B = 'B'
fromAlphaChar C = 'C'
fromAlphaChar D = 'D'
fromAlphaChar E = 'E'
fromAlphaChar F = 'F'
fromAlphaChar G = 'G'
fromAlphaChar H = 'H'
fromAlphaChar I = 'I'
fromAlphaChar J = 'J'
fromAlphaChar K = 'K'
fromAlphaChar L = 'L'
fromAlphaChar M = 'M'
fromAlphaChar N = 'N'
fromAlphaChar O = 'O'
fromAlphaChar P = 'P'
fromAlphaChar Q = 'Q'
fromAlphaChar R = 'R'
fromAlphaChar S = 'S'
fromAlphaChar T = 'T'
fromAlphaChar U = 'U'
fromAlphaChar V = 'V'
fromAlphaChar W = 'W'
fromAlphaChar X = 'X'
fromAlphaChar Y = 'Y'
fromAlphaChar Z = 'Z'

alphabetCipher :: String -> String -> String
alphabetCipher message keyword =  maybe "" (transformMessage [A .. Z] message . cycle) (sequence (map toAlphaChar keyword)) 

alphabetDecipher :: String -> String -> String
alphabetDecipher message keyword = maybe "" (transformMessage [Z, Y .. A] message . cycle) (sequence (map toAlphaChar keyword)) 
 
transformMessage :: [AlphaChar] -> String -> [AlphaChar] -> String 
transformMessage _ [] _ = ""
transformMessage _ _ [] = ""
transformMessage range (m : ms) (k : ks) =  maybe (m : transformMessage range ms (k : ks)) 
                                  ((: (transformMessage range ms ks)) . restoreCase m . rotateToNewChar range k) 
                                  (toAlphaChar m)
                            where restoreCase c transformedC 
                                    | isLower c = toLower transformedC
                                    | otherwise = transformedC 
           
rotateToNewChar :: [AlphaChar] -> AlphaChar -> AlphaChar -> Char
rotateToNewChar range k c = 
  (fromAlphaChar . (!! (fromEnum k - fromEnum A)) . dropWhile (/= c) . cycle $ range) 


alphabetCipherTest :: IO ()
alphabetCipherTest = do testEncoding "MPPR AE OYWY"  "MEET AT DAWN" "ALLY"
                        testEncoding  "egsgqwtahuiljgs" "meetmebythetree" "scones"
                        testEncoding  "eGsgqwtahuIljgs" "mEetmebythEtree" "scones"
                        testDecoding  "MEET AT DAWN" "MPPR AE OYWY" "ALLY"
                        testDecoding  "meetmebythetree" "egsgqwtahuiljgs" "scones"
                        testDecoding  "mEetmebythEtree" "eGsgqwtahuIljgs" "scones"

testEncoding :: String -> String -> String -> IO()
testEncoding expectedEncoding message keyword = 
  if encoded == expectedEncoding
    then putStrLn ("encoding pass: " ++ message ++ " -> " ++ keyword ++ " -> " ++ encoded) 
  else putStrLn ("encoding fail: "  ++ message ++ " -> " ++ keyword ++ " ->  " ++ encoded) where
   encoded = alphabetCipher message keyword

testDecoding :: String -> String -> String -> IO()
testDecoding expectedDecoding message keyword = 
  if decoded == expectedDecoding
    then putStrLn ("decoding pass: " ++ message ++ " -> " ++ keyword ++ " -> " ++ decoded) 
  else putStrLn ("decoding fail: "  ++ message ++ " -> " ++ keyword ++ " ->  " ++ decoded) where
   decoded = alphabetDecipher message keyword
