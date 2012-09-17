import Text.Regex.Base
import Text.Regex.Posix
import Data.Char

main = do
  let bs = simpleBars $ replicate 24 ' '
  let bs' = replaceAt 8 'T' bs
  let results = take 31 $ progress bs'
  --mapM putStrLn results

  let bs = simpleBars $ replicate 78 ' '
  let pos = 0
  let acc = 1
  let accx = 1
  let s = ""
  let p = ""
  let o = ""
  let commands = "1(///(1iTiTiTi|||[(1 ])1( [L|[L|[L|[(] |1//)/)1i||1)///)1i||||1(///)1i(/////)1iTiTi[L!])|])[L!])])l|])1/( [(1/ ]L!l|[(1 ])1( //(1 ]L[L!|"

  let (pos', acc', accx', bs', s', p', o') = foldl execute (pos, acc, accx, bs, s, p, o) commands
  putStrLn p'
  putStrLn o'

simpleBars :: String -> String
simpleBars line = line

replaceAt :: Int -> Char -> String -> String
replaceAt i c origin = before ++ c:after
  where before = take i origin
        after = drop (i+1) origin

nextChar :: (Char, Char, Char) -> Char
nextChar ('i', 'T', 'i') = 'i'
nextChar ('i', 'T',  _ ) = ' '
nextChar ( _ , 'T', 'i') = ' '
nextChar ( _ , 'T',  _ ) = 'i'
nextChar ('i', ' ', 'i') = ' '
nextChar ('i', ' ',  _ ) = 'i'
nextChar ( _ , ' ', 'i') = 'i'
nextChar ( _ , ' ',  _ ) = ' '
nextChar ( _ , 'i',  _ ) = 'T'

next :: String -> String
next s = map (nextChar . toSeed) [0..(len-1)]
  where len = length s
        toSeed i = (s !! ((i-1+len) `mod` len), s !! i, s !! ((i+1) `mod` len))

progress :: String -> [String]
progress s = s:progress (next s)


execute :: (Int, Int, Int, String, String, String, String) -> Char -> (Int, Int, Int, String, String, String, String)
execute (pos, acc, accx, bs, s, p, o) '1' = (pos, acc', accx, bs, s, p, o)
  where acc' = 1
execute (pos, acc, accx, bs, s, p, o) '/' = (pos, acc', accx, bs, s, p, o)
  where acc' = acc * 2
execute (pos, acc, accx, bs, s, p, o) ')' = (pos', acc, accx, bs, s, p, o)
  where pos' = (pos+acc) `mod` (length bs)
execute (pos, acc, accx, bs, s, p, o) '(' = (pos', acc, accx, bs, s, p, o)
  where pos' = (pos-acc) `mod` (length bs)
execute (pos, acc, accx, bs, s, p, o) c
  | c == 'i' || c == 'T' || c == ' ' = (pos', acc, accx, bs', s, p, o)
  where new_bs pos_ bs_ = replaceAt pos_ c bs_
        new_pos pos_ = (pos_+1) `mod` (length bs)
        (bs', pos') = foldl (\(bs_, pos_) _ -> (new_bs pos_ bs_, new_pos pos_)) (bs, pos) [1..acc] -- [0..acc]
execute (pos, acc, accx, bs, s, p, o) ']' = (pos, acc', accx, bs, s', p, o)
  where s' = (drop pos bs) ++ (take (pos+1) bs)
        (_, end) = (s' =~ "^ *[iT]* " :: (MatchOffset,MatchLength))
        acc' = if end == 0
               then 0
               else end - 1
execute (pos, acc, accx, bs, s, p, o) '[' = (pos, acc', accx, bs, s', p, o)
  where s' = (bs !! (pos-1)):(drop pos bs) ++ (take pos bs)
        (start, _) = (s' =~ " [iT]* *$" :: (MatchOffset,MatchLength))
        acc' = if start == -1
               then 0
               else (length s') - start - 1
execute (pos, acc, accx, bs, s, p, o) 'l' = (pos, acc', accx', bs, s, p, o)
  where acc' = accx
        accx' = acc
execute (pos, acc, accx, bs, s, p, o) 'L' = (pos, acc', accx', bs, s, p, o)
  where acc' = accx - acc
        accx' = accx + acc
execute (pos, acc, accx, bs, s, p, o) '|' = (pos, acc, accx, bs', s, p', o)
  where p' = p ++ '\n':bs
        bs' = next bs
execute (pos, acc, accx, bs, s, p, o) '!' = (pos, acc, accx, bs, s, p, o')
  where o' = o ++ [chr (((ord '0') + acc) `mod` 128)]
