-- CHAPTER 11. ALGEBRAIC DATATYPES
module Chapter11 where

data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini | Mazda | Honda deriving (Eq, Show)

data Airline =
  Ana | United | Thai deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price | Plane Airline deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

main :: IO ()
main = do
  putStrLn $ show $ isCar $ Car Honda $ Price 100
  putStrLn $ show $ isPlane $ Car Honda $ Price 100
  putStrLn $ show $ areCars $ [Car Honda $ Price 100, Plane United]
