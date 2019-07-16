module Main ( main ) where

import Ulme
import qualified Parse.Json as Json
import qualified System.IO as IO
import Text.Pretty.Simple ( pPrint )


{- TODO

    * Json Strings bremsen enorm.

-}


main =
    "[1,2,3,4]"
    |> Json._json
    |> pPrint


