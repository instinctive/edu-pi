module Pi ( Calc(..), darts, leibniz, parse ) where

import Prelude hiding ( pi )
import Control.Monad.Random.Class ( MonadRandom, getRandomR )

-- An iterative monadic calculation, so we can report in occasionally.
newtype Calc m = Calc ( m ( Double, Calc m ) )

-- ----------------------------------------------------------------------
-- The Leibniz formula: pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...

leibniz :: Monad m => Calc m
leibniz = Calc $ go (0::Int) 0 where
    go !n !pi4 = pure ( 4*pi4, Calc $ go (n+1) (pi4 + term) ) where
        term = num / fromIntegral (n*2 + 1)
        num = if even n then 1 else -1

-- ----------------------------------------------------------------------
-- Throw darts at a bounding box and see which end up inside a 0.5 radius.

darts :: MonadRandom m => Calc m
darts = Calc $ go (0::Int) (0::Int) where
    go !h !t = do
        h' <- bool h (h+1) <$> trial
        pure (area h t, Calc $ go h' (t+1))

-- Area of a radius 0.5 circle from hits and trials within the bounding box.
area :: Int -> Int -> Double
area h t = 4 * fromIntegral h / fromIntegral t

-- Is a random point in the bounding box within a 0.5 radius circle?
trial :: MonadRandom m => m Bool
trial = inCircle <$> rnd <*> rnd where
    rnd = subtract (0.5 :: Double) <$> getRandomR (0,1)
    inCircle x y = x*x + y*y <= 0.25

-- ----------------------------------------------------------------------

parse :: MonadRandom m => [String] -> Calc m
parse ["leibniz"] = leibniz
parse ["darts"]   = darts
parse s = error $ "invalid arguments: " <> show s
