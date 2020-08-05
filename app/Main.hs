module Main ( main ) where

import Prelude hiding     ( pi      )
import System.Environment ( getArgs )

import Pi     ( Calc(Calc), parse )
import Report ( mkReporter        )

main :: IO ()
main = do
    calc <- parse <$> getArgs
    reporter <- mkReporter

    let go !z !n (Calc m) = do
            (pi, calc') <- m
            z' <- if z == n
                then reporter n pi >> pure (z*2)
                else pure z
            go z' (n+1) calc'

    go (1::Int) 0 calc
