import Data.List

main = do
  let lttb m = lets_take_tea_break m 17 3569 915
  let Just answer = find (/= "") $ map lttb [0..]
  putStrLn answer

lets_take_tea_break :: Integer -> Integer -> Integer -> Integer -> String
lets_take_tea_break m e n c
  | (m ^ e) `mod` n == c = show m
  | otherwise = ""
