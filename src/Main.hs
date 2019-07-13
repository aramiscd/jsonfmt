module Main ( main ) where

import Ulme
import Ulme.Result ( Result ( Err, Ok ) )

import qualified System.IO as IO

import Parse.Json



main =
    json "[1]"
    |> show
    |> IO.putStrLn


