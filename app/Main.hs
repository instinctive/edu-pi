module Main ( main ) where

import Prelude hiding     ( pi      )
import System.Environment ( getArgs )

import Pi     ( Calc(Calc), parse )
import Report ( mkReporter        )

main :: IO ()
main = do
    calc <- parse <$> getArgs
    reporter <- mkReporter

    let go z n (Calc m) = do
            (pi, calc') <- m
            if z == n then do
                reporter n pi
                go (z*2) (n+1) calc'
            else go z (n+1) calc'

    go 1 0 calc
