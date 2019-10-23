module Main

{-
    Copyright 2019, Aramis Concepcion Duran
    
    This file is part of jsonfmt.

    Jsonfmt is free software: you can redistribute it
    and/or modify it under the terms of the GNU General
    Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at
    your option) any later version.

    Jsonfmt is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the
    implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General
    Public License along with Foobar.  If not, see
    <https://www.gnu.org/licenses/>.


    -------------------------------------------------------


    Read a JSON document from stdin, parse it into a
    `Json` value, apply a pretty-printer to it and write
    the resulting string to the standard output.
-}

( main
)
where


import Ulme

import qualified Ulme.IO        as IO

import qualified Json.Parse     as Parse
import qualified Json.Pretty    as Pretty

import System.Exit  ( ExitCode ( ExitFailure ) , exitWith )


main :: IO ()
main =
    IO.getContents IO.stdin
    |> andThen
        ( \ case
            Ok contents ->
                case Parse.json contents of
                    Ok ( json , [] ) ->
                        IO.println ( Pretty.print json )

                    Ok ( _ , pending ) ->
                        IO.printErr ( "Invalid JSON: " ++ pending )
                        >>> exitWith ( ExitFailure 1 )

                    Err error ->
                        IO.printErr ( show error )
                        >>> exitWith ( ExitFailure 1 )

            Err error ->
                IO.printErr ( show error )
                >>> exitWith ( ExitFailure 1 )
        )
