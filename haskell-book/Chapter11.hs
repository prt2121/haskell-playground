-- CHAPTER 11. ALGEBRAIC DATATYPES
module Chapter11 where

data Price =
  Price Integer deriving (Eq, Show)

data Size =
  Size Integer deriving (Eq, Show)

data Manufacturer =
  Mini | Mazda | Honda deriving (Eq, Show)

data Airline =
  Ana | United | Thai deriving (Eq, Show)

data Vehicle =
  Car Manufacturer Price | Plane Airline deriving (Eq, Show)

data Vehicle' =
  Car' Manufacturer Price | Plane' Airline Size deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> [Manufacturer]
getManu (Car m _) = [m]
getManu _         = []

size :: Vehicle' -> Maybe Integer
size (Plane' _ (Size s)) = Just s
size _                   = Nothing

main :: IO ()
main = do
  putStrLn $ show $ isCar $ Car Honda $ Price 100
  putStrLn $ show $ isPlane $ Car Honda $ Price 100
  putStrLn $ show $ areCars $ [Car Honda $ Price 100, Plane United]
  putStrLn $ show $ getManu $ Car Honda $ Price 100
  putStrLn $ show $ getManu $ Plane United
  putStrLn $ show $ Plane' United $ Size 700
  putStrLn $ show $ size $ Plane' United $ Size 700
