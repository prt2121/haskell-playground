module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = state $ randomR (1, 6) >>= \(n, s) -> return (intToDie n, s)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a
-- :t randomR (1, 6)
-- randomR (1, 6) :: (Num a, RandomGen g, Random a) => g -> (a, g)

rollDie'' :: State StdGen Die
rollDie'' = intToDie <$> state (randomR (1, 6))

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (c, ls) gen
      | sum >= n = (c, ls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (c + 1, ls ++ [(intToDie die)]) nextGen

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi m) =
    Moi $ \s -> mapFst f (m s)
            where
              mapFst f (x, y) = (f x, y)

instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  -- (<*>) :: Moi s (a -> b)
  --       -> Moi s a
        -- -> Moi s b
  (<*>) (Moi ab) (Moi a) =
    Moi $ \sa
    -> let (x, sb) = ab sa
           (y, s) = a sb
       in (x y, s)

main :: IO ()
main = do
  putStrLn "hello world"
