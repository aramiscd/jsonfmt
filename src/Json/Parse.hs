module Json.Parse

{-
    A JSON-parsing module.

    This module provides a data type for representing
    JSON documents and a dedicated parser function based
    on simple parser-combinators.
-}

( Json ( Jatom, Jarray, Jobject )
, json
)
where


import Ulme
import qualified Ulme.Char as Char
import qualified Ulme.List as List
import qualified Ulme.String as String

import Json ( Json ( Jatom, Jarray, Jobject ) )
import Parse ( Parser )
import qualified Parse


json :: Parser Json
{-
    Parse an entire JSON document into a `Json` value.
-}
json =
    element
    >> \ case
        Err error            -> Err error
        Ok ( done, "" )      -> Ok ( done, "" )
        Ok ( done, pending ) -> Err ( "Invalid JSON", pending )
 
        


value :: Parser Json
{-
    Parse a JSON value into a `Json` value.
-}
value =
    Parse.oneOf
        [ object
        , array
        , string
        , number
        , jatom ( Parse.string "true" )
        , jatom ( Parse.string "false" )
        , jatom ( Parse.string "null" )
        ]


jatom :: Parser [ String ] -> Parser Json
{-
    Turn a `[ String ]` parser into a `Json` parser that
    produces a `Jatom`.

    This is basically a type-shim.
-}
jatom =
    Parse.map
        ( List.foldr (++) [] >> Jatom )


object :: Parser Json
{-
    Parse a JSON object into a `Jobject`.
-}
object =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "{" )
        , Parse.oneOf [ members, Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "}" )
        ]
    |> Parse.map Jobject


members :: Parser [ ( Json, Json ) ]
{-
    Parse the members of a JSON object.
-}
members =
    Parse.sequence
        [ Parse.map List.singleton member
        , Parse.optional <|
            Parse.sequence
                [ Parse.throwAway ( Parse.string "," ), members ]
        ]


member :: Parser ( Json, Json )
{-
    Parse a single member of a JSON object.
-}
member =
    Parse.sequence
        [ Parse.throwAway whitespace
        , Parse.map List.singleton string
        , Parse.throwAway whitespace
        , Parse.throwAway ( Parse.string ":" )
        , Parse.map List.singleton element
        ]
    >> \ case
        Err error                 -> Err error
        Ok ( [ k, v ] , pending ) -> Ok ( ( k, v ), pending )
        Ok ( values, pending )    -> Err ( show values, pending )


array :: Parser Json
{-
    Parse a JSON array into a `Jarray`.
-}
array =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "[" )
        , Parse.oneOf [ elements, Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "]" )
        ]
    |> Parse.map Jarray


elements :: Parser [ Json ]
{-
    Parse the elements of a JSON array.
-}
elements =
    Parse.sequence
        [ Parse.map List.singleton element
        , Parse.optional <|
            Parse.sequence
                [ Parse.throwAway ( Parse.string "," ), elements ]
        ]


element :: Parser Json
{-
    Parse a single element of a JSON array.
-}
element =
    Parse.sequence
        [ Parse.throwAway whitespace
        , Parse.map List.singleton value
        , Parse.throwAway whitespace
        ]
    >> \ case
        Err error              -> Err error
        Ok ( [ v ] , pending ) -> Ok ( v, pending )
        Ok ( values, pending ) -> Err ( show values, pending )


string :: Parser Json
{-
    Parse a JSON string into a `Jatom`.
-}
string =
    jatom <|
        Parse.sequence
            [ Parse.string "\"", characters, Parse.string "\"" ]


characters :: Parser [ String ]
{-
    Parse any number of characters.
-}
characters  =
    Parse.zeroOrMore character


char :: Parser [ String ]
{-
    Parse a valid unescaped single character.
-}
char input =
    let error = Err ( "Expect valid char", input ) in
    case input of
        ( head : tail ) ->
            let code = Char.toCode head in
            if code == 34 || code == 92 || code < 32 || code > 1114111
            then error else Ok ( [ String.fromChar head ], tail )
        _ -> error


character :: Parser [ String ]
{-
    Parse a valid single character.
-}
character =
    Parse.oneOf
        [ char, Parse.sequence [ Parse.string "\\", escape ] ]


escape :: Parser [ String ]
{-
    Parse an escape character.
-}
escape =
    Parse.oneOf
        [ Parse.string "\""
        , Parse.string "\\"
        , Parse.string "/"
        , Parse.string "b"
        , Parse.string "f"
        , Parse.string "n"
        , Parse.string "r"
        , Parse.string "t"
        , Parse.sequence [ Parse.string "u", hex, hex, hex, hex ]
        ]


hex :: Parser [ String ]
{-
    Parse a hex digit.
-}
hex =
    Parse.oneOf
        [ digit
        , Parse.oneOf
            [ Parse.string "A"
            , Parse.string "B"
            , Parse.string "C"
            , Parse.string "D"
            , Parse.string "E"
            , Parse.string "F"
            , Parse.string "a"
            , Parse.string "b"
            , Parse.string "c"
            , Parse.string "d"
            , Parse.string "e"
            , Parse.string "f"
            ]
        ]


number :: Parser Json
{-
    Parse a JSON number.

    JSON numbers a either integer or floating-point,
    with or without a scientific exponential suffix.
    Only decimal notation is valid; no hex, no octal.
-}
number =
    jatom <|
        Parse.sequence
            [ integer, fraction, exponent ]


integer :: Parser [ String ]
{-
    Parse an integer.

    Only decimal notation, no leading zeros, with or
    without a leading minus.
-}
integer =
    Parse.oneOf
        [ Parse.sequence [ onenine, digits ]
        , digit
        , Parse.sequence [ Parse.string "-", onenine, digits ]
        , Parse.sequence [ Parse.string "-", digit ]
        ]


digits :: Parser [ String ]
{-
    Parse any number of decimal digits.
-}
digits =
    Parse.oneOrMore digit


digit :: Parser [ String ]
{-
    Parse a single decimal digit.
-}
digit =
    Parse.oneOf [ Parse.string "0", onenine ] 


onenine :: Parser [ String ]
{-
    Parse a non-zero decimal digit.
-}
onenine =
    Parse.oneOf
        [ Parse.string "1"
        , Parse.string "2"
        , Parse.string "3"
        , Parse.string "4"
        , Parse.string "5"
        , Parse.string "6"
        , Parse.string "7"
        , Parse.string "8"
        , Parse.string "9"
        ]


fraction :: Parser [ String ]
{-
    Parse the fractional part of a floating-point number.
-}
fraction =
    Parse.optional <|
        Parse.sequence
            [ Parse.string ".", digits ]


exponent :: Parser [ String ]
{-
    Parse the scientific exponential suffix of a number
    (if any).
-}
exponent =
    Parse.optional <|
        Parse.sequence
            [ Parse.oneOf [ Parse.string "E", Parse.string "e" ]
            , sign
            , digits
            ]


sign :: Parser [ String ]
{-
    Parse the sign of a scientific exponential suffix.

    This is NOT the optional sign in front JSON number.
-}
sign =
    Parse.optional <|
        Parse.oneOf
            [ Parse.string "+", Parse.string "-" ]


whitespace :: Parser [ String ]
{-
    Parse whitespace (greedy).
-}
whitespace =
    Parse.zeroOrMore <|
        Parse.oneOf
            [ Parse.string "\x0020"
            , Parse.string "\x000D"
            , Parse.string "\x000A"
            , Parse.string "\x0009"
            ]
