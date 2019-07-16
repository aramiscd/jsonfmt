{-
    This module provides the `Json` data type: a simple,
    well-typed Haskell-representation of JSON documents.
-}

module Json
    ( Json ( Jatom, Jarray, Jobject )
    )
where

import Ulme


data Json
{-
    A data type for representing JSON Documents.
-}
    = Jatom String
    | Jarray [ Json ]
    | Jobject [ ( Json, Json ) ]
    deriving Show
