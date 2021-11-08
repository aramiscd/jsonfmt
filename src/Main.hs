{-
    Read a JSON document from stdin, parse it into a
    `Json` value, apply a pretty-printer to it and write
    the resulting string to the standard output.

    ----

    Copyright 2019-2020, Aramis Concepcion Duran

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
-}

module Main (main) where

import Ulme

import System.Exit (ExitCode (ExitFailure), exitWith)
import Ulme.IO qualified as IO
import Ulme.Json.Parse qualified as Parse
import Ulme.Json.Pretty qualified as Pretty


main :: IO ()
main =
    IO.getContents IO.stdin >>= \ case
        Ok contents ->
            case Parse.json contents of
                Just json -> IO.println (Pretty.print json)
                Nothing -> exitWith (ExitFailure 1)
        Err ioException -> do
            IO.printErr (show ioException)
            exitWith (ExitFailure 1)
