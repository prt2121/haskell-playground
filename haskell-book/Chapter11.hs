-- CHAPTER 11. ALGEBRAIC DATATYPES
module Chapter11 where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Char (toUpper)
import Data.Foldable (toList)

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

data OS =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data Lang =
  Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OS, lang :: Lang } deriving (Eq, Show)

allOSs :: [OS]
allOSs = [ GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows ]

allLangs :: [Lang]
allLangs = [Haskell, Agda, Idris, PureScript]

-- p. 415
-- Write a function that generates all possible values of Programmer.
allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = o, lang = l} | o <- allOSs, l <- allLangs ]

allProgrammers' :: [Programmer]
allProgrammers' = (\o l -> Programmer { os = o, lang = l}) <$> allOSs <*> allLangs

allProgrammers'' :: [Programmer]
allProgrammers'' = liftA2 (\o l -> Programmer { os = o, lang = l}) allOSs allLangs
-- liftA2 Lift a binary function to actions.

-- Language exercises
capitalizeWord :: String -> String
capitalizeWord ls = (toUpper . head) ls : tail ls

capitalizeWord' :: String -> String
capitalizeWord' = ap ((:) . toUpper . head) tail

-- ap :: Monad m => m (a -> b) -> m a -> m b
-- In many situations, the liftM operations can be replaced by uses of ap, which promotes function application.
-- > return f `ap` x1 `ap` ... `ap` xn
-- is equivalent to
-- > liftMn f x1 x2 ... xn

-- very bad implementation but i'm lazy now
capitalizeParagraph :: String -> String
capitalizeParagraph s = init $ flatMap ((++ " ") . capitalizeWord) (sentence s)

sentence :: String -> [String]
sentence s =  case dropWhile (== '.') s of
                      "" -> []
                      s' -> (w ++ ".") : sentence (drop 2 s'')
                            where (w, s'') = break (== '.') s'

flatMap f = concatMap (toList . f)

main :: IO ()
main = do
  putStrLn $ show $ isCar $ Car Honda $ Price 100
  putStrLn $ show $ isPlane $ Car Honda $ Price 100
  putStrLn $ show $ areCars $ [Car Honda $ Price 100, Plane United]
  putStrLn $ show $ getManu $ Car Honda $ Price 100
  putStrLn $ show $ getManu $ Plane United
  putStrLn $ show $ Plane' United $ Size 700
  putStrLn $ show $ size $ Plane' United $ Size 700
