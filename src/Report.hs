module Report ( mkReporter ) where

import Formatting       ( (%), fprint, float, commas )
import Formatting.Clock ( timeSpecs                  )
import System.Clock     ( Clock(Monotonic), getTime  )

mkReporter :: IO (Int -> Double -> IO ())
mkReporter = mk <$> getTime Monotonic where
    mk start n v = do
        now <- getTime Monotonic
        fprint
            ( float % " after "
            % commas % " iterations in "
            % timeSpecs % "\n"
            ) v n start now
