module AlphabetCipherTest where
import AlphabetCipher

alphabetCipherTest :: IO ()
alphabetCipherTest = do testEncoding Nothing  "MEET AT DAWN" "A90LLY"
                        testEncoding (Just "MPPR AE OYWY")  "MEET AT DAWN" "ALLY"
                        testEncoding (Just "egsgqwtahuiljgs") "meetmebythetree" "scones"
                        testEncoding (Just "eGsgqwtahuIljgs") "mEetmebythEtree" "scones"
                        testDecoding (Just "MEET AT DAWN") "MPPR AE OYWY" "ALLY"
                        testDecoding (Just "meetmebythetree") "egsgqwtahuiljgs" "scones"
                        testDecoding (Just "mEetmebythEtree") "eGsgqwtahuIljgs" "scones"

testEncoding :: Maybe String -> String -> String -> IO()
testEncoding expectedEncoding message keyword = 
  if encoded == expectedEncoding
    then putStrLn ("encoding pass: " ++ message ++ " -> " ++ keyword ++ " -> " ++ show encoded) 
  else putStrLn ("encoding fail: "  ++ message ++ " -> " ++ keyword ++ " ->  " ++ show encoded) where
   encoded = alphabetCipher message keyword

testDecoding :: Maybe String -> String -> String -> IO()
testDecoding expectedDecoding message keyword = 
  if decoded == expectedDecoding
    then putStrLn ("decoding pass: " ++ message ++ " -> " ++ keyword ++ " -> " ++ show decoded) 
  else putStrLn ("decoding fail: "  ++ message ++ " -> " ++ keyword ++ " ->  " ++ show decoded) where
   decoded = alphabetDecipher message keyword
