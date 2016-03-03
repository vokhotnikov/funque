import Test.Framework

import System.Exit(exitFailure)
import Control.Monad(unless)

import qualified StompParserSpec 

main :: IO ()
main = defaultMain $ StompParserSpec.tests
                   

