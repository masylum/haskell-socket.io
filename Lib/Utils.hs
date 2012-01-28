module Lib.Utils (
  split
) where

split :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                          where (w, s'') = break p s'

unpack :: [Int]
unpack = fold (\x n -> (n * 256) + x ) 0

padl :: String -> Int -> Char -> String
padl s n c = (++) . take (length s) (repeat c) $ s
