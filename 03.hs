import qualified Data.Map as Map

main = do
  let bs = simpleBars $ "I    IT ii  i I   I i   i   I  T"
  let results = take 27 $ progress bs
  mapM putStrLn results
  putStrLn $ decode_morse $ last results

simpleBars :: String -> String
simpleBars line = line

replaceAt :: Int -> Char -> String -> String
replaceAt i c origin = before ++ c:after
  where before = take i origin
        after = drop (i+1) origin

nextChar :: (Char, Char, Char) -> Char
nextChar (left, center, right) = result
  where table =  [Map.fromList[(' ',' '), ('i','T'), ('T','i'), ('I','I')],
                  Map.fromList[(' ','i'), ('i','I'), ('T',' '), ('I','T')]]
        valueOf c = if c == 'i' || c == 'I' then 1 else 0
        index = ((valueOf left) + (valueOf right)) `mod` 2
        (Just result) = Map.lookup center (table !! index)

next :: String -> String
next s = map (nextChar . toSeed) [0..(len-1)]
  where len = length s
        toSeed i = (s !! ((i-1+len) `mod` len), s !! i, s !! ((i+1) `mod` len))

progress :: String -> [String]
progress s = s:progress (next s)

decode_morse :: String -> String
decode_morse = map decode_morses_code . words

decode_morses_code :: String -> Char
decode_morses_code code = c
  where morseTable = Map.fromList [
          ("iI"  , 'A'),
          ("Ii"  , 'N'),
          ("Iiii", 'B'),
          ("III" , 'O'),
          ("IiIi", 'C'),
          ("iIIi", 'P'),
          ("Iii" , 'D'),
          ("IIiI", 'Q'),
          ("i"   , 'E'),
          ("iIi" , 'R'),
          ("iiIi", 'F'),
          ("iii" , 'S'),
          ("IIi" , 'G'),
          ("I"   , 'T'),
          ("iiii", 'H'),
          ("iiI" , 'U'),
          ("ii"  , 'I'),
          ("iiiI", 'V'),
          ("iIII", 'J'),
          ("iII" , 'W'),
          ("IiI" , 'K'),
          ("IiiI", 'X'),
          ("iIii", 'L'),
          ("IiII", 'Y'),
          ("II"  , 'M'),
          ("IIii", 'Z')]
        result = flip Map.lookup morseTable $ code
        Just c = if result == Nothing
                 then Just '_'
                 else result
