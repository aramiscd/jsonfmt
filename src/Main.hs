module Main

{-
    Parse a JSON document into a value of type `Json`,
    apply a pretty-printer to it and write the resulting
    string to the standard output.
-}

( main
)
where


import Ulme
import qualified Parse

import qualified Json.Parse as Parse
import qualified Json.Pretty as Pretty

import System.Exit ( ExitCode ( ExitFailure ), exitWith )
import qualified System.IO as IO


main :: IO ()
main =
    "[1,2,3,4]"
    |> Parse.json
    |> \ case
        Ok ( json, [] ) ->
            IO.putStrLn ( Pretty.print json )

        Ok ( json, pending ) ->
            IO.hPutStrLn IO.stderr ( "Invalid JSON: " ++ pending )
            >>> exitWith ( ExitFailure 1 )

        Err error ->
            IO.hPutStrLn IO.stderr ( show error )
            >>> exitWith ( ExitFailure 1 )
