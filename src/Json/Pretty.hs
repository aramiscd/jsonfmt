module Json.Pretty

{-
    Pretty-print a JSON document.

    This module provides a `print` function which turns
    a JSON document (provided as a `Json` value) into a
    "well-formatted" string representation.  My motivation
    for writing this pretty-printer was twofold:

    (a) I needed an excuse to figure out how to write
        a JSON parser.

    (b) I like how the `elm-format` code formatter
        outlines lists and similar structures with commas
        as prefixes:

            [ foo
            , bar
            , baz
            ]

        instead of:

            [ foo,
              bar,
              baz
            ]

        I prefer the former style and would like to apply
        it to JSON documents.  To my best knowledge, no
        readily available JSON pretty-printer supported
        this style at the time.  So, I wrote my own.

    Right now, there is exactly one output for every
    set of semantically equivalent JSON documents.
    Input whitespace is not taken into account at all.
    I might or might not change that in future versions
    of this program.
-}

( print
)
where


import Ulme
import qualified Ulme.String as String

import Json ( Json ( Jatom, Jarray, Jobject ) )


print :: Json -> String
{-
    Pretty-print a JSON value.
-}
print =
    printValue >> String.join "\n"


printValue :: Json -> [ String ]
{-
    Pretty-print a JSON value as a list strings.
-}
printValue = \ case
    Jatom atom      -> [ atom ]
    Jarray elements -> printArray elements
    Jobject members -> printObject members


printArray :: [ Json ] -> [ String ]
{-
    Pretty-print a JSON array as a list of strings.
-}
printArray = \ case
    [] -> [ "[]" ]
    firstElement : elements ->
        printElement True firstElement
        ++ ( elements >>= printElement False )
        ++ [ "]" ]


printElement :: Bool -> Json -> [ String ]
{-
    Pretty-print a JSON array element as a list of strings.
-}
printElement isFirst element =
    let prefix = if isFirst then "[ " else  ", " in
    printValue element |> \ case
        [] -> []
        firstLine : lines ->
            [ prefix ++ firstLine ] ++ map ( "  " ++ ) lines


printObject :: [ ( Json, Json ) ] -> [ String ]
{-
    Pretty-print a JSON object as a list of strings.
-}
printObject = \ case
    [] -> [ "{}" ]
    firstMember : members ->
        printMember True firstMember
        ++ ( members >>= printMember False )
        ++ [ "}" ]


printMember :: Bool -> ( Json, Json ) -> [ String ]
{-
    Pretty-print a JSON object member as a list of strings.
-}
printMember isFirst ( key, value ) =
    let prefix = if isFirst then "{ " else  ", " in
    printValue key
    |> \ case
        [] -> []
        k : _ ->
            [ prefix ++ k ++ " :" ]
            ++ map ( "    " ++ ) ( printValue value )
