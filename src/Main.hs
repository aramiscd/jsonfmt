module Main ( main ) where

import Ulme
import qualified Json.Parse as Parse
import qualified System.IO as IO
import Text.Pretty.Simple ( pPrint )


main =
    "[1,2,3,4]"
    |> Parse.json
    |> pPrint
