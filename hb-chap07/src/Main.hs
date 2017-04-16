module Main where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral t => t -> t
tensDigit' x   = d
  where (f, _) = divMod x 10
        (_, d) = divMod f 10

tensDigit'' :: Integer -> Integer
tensDigit'' = (flip mod 10) . (flip div 10)

-- hunsD :: Integral a => t -> a
hunsD :: Integral t => t -> t
hunsD x = d
  where h = x `div` 100
        d = h `mod` 10

hunsD' :: (Integral b, Show b)
  => b
  -> Either String b
hunsD' x =
  if x < 100
  then Left $ show x
  else Right $ hunsD x

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> x
    False -> y

g :: (a -> b) -> (a, c) -> (b, c)
g ab (a, c) = (ab a, c)

roundTrip :: (Show a, Read a)
  => a
  -> a
roundTrip = read . show

main :: IO ()
main = do
  print ((roundTrip 4) :: Int)
