module Json

{-
    Copyright 2019, Aramis Concepcion Duran
    
    This file is part of jsonfmt.

    Ulme is free software: you can redistribute it and/or
    modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation,
    either version 3 of the License, or (at your option)
    any later version.

    Ulme is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the
    implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General
    Public License along with Foobar.  If not, see
    <https://www.gnu.org/licenses/>.


    -------------------------------------------------------


    This module provides the `Json` data type: a simple,
    well-typed Haskell-representation of JSON documents.
-}

( Json ( Jatom , Jarray , Jobject )
)
where


import Ulme


data Json
{-
    A data type for representing JSON Documents.
-}
    = Jatom String
    | Jarray [ Json ]
    | Jobject [ ( Json , Json ) ]
    deriving Show
