module Json.Parse

{-
    A JSON-parsing module.

    This module provides a parser function for JSON
    documents, based on simple parser-combinators.

    I think, it is best to read this module from top
    to bottom.  Simple parsers are introduced first.


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

( Json ( Jatom , Jarray , Jobject )
, json
)
where


import Ulme

import qualified Ulme.Char      as Char
import qualified Ulme.List      as List
import qualified Ulme.String    as String

import qualified Parse

import Json     ( Json ( Jatom , Jarray , Jobject ) )
import Parse    ( Parser )


whitespace :: Parser ( List String )
{-
    Parse whitespace (greedy).
-}
whitespace =
    Parse.zeroOrMore
        ( Parse.oneOf
            [ Parse.string "\x0020"
            , Parse.string "\x000D"
            , Parse.string "\x000A"
            , Parse.string "\x0009"
            ]
        )


-- Integers ---------------------------------------------------------


onenine :: Parser ( List String )
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


digit :: Parser ( List String )
{-
    Parse a decimal digit.
-}
digit =
    Parse.oneOf
        [ Parse.string "0"
        , onenine
        ] 


digits :: Parser ( List String )
{-
    Parse as many decimal digits as possible but at
    least one.
-}
digits =
    Parse.oneOrMore digit


integer :: Parser ( List String )
{-
    Parse an integer.

    Only decimal notation, no leading zeros, with or
    without a leading minus.
-}
integer =
    Parse.oneOf
        [ Parse.sequence [ onenine , digits ]
        , digit
        , Parse.sequence [ Parse.string "-" , onenine , digits ]
        , Parse.sequence [ Parse.string "-" , digit ]
        ]


-- Floating-point numbers -------------------------------------------


fraction :: Parser ( List String )
{-
    Parse the fractional part of a floating-point number.
-}
fraction =
    Parse.sequence
        [ Parse.string "."
        , digits
        ]


sign :: Parser ( List String )
{-
    Parse the sign of a scientific exponential suffix.

    This is NOT the optional sign in front of a JSON
    number.
-}
sign =
    Parse.optional
        ( Parse.oneOf
            [ Parse.string "+"
            , Parse.string "-"
            ]
        )


exponent :: Parser ( List String )
{-
    Parse the scientific exponential suffix of a number
    (if any).
-}
exponent =
    Parse.sequence
        [ Parse.oneOf [ Parse.string "E" , Parse.string "e" ]
        , sign
        , digits
        ]


number :: Parser ( List String )
{-
    Parse a JSON number.

    JSON numbers are either integer or floating-point,
    with or without a scientific exponential suffix.
    Only decimal notation is valid: no hex, no octal.
-}
number =
    Parse.sequence
        [ integer
        , Parse.optional fraction
        , Parse.optional exponent
        ]
    |> Parse.withError "Expecting a number"


-- Strings ----------------------------------------------------------


hex :: Parser ( List String )
{-
    Parse a hexadecimal digit.
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


escape :: Parser ( List String )
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
        , Parse.sequence [ Parse.string "u" , hex , hex , hex , hex ]
        ]


char :: Parser ( List String )
{-
    Parse an unescaped character.
-}
char input =
    case input of
        "" -> Err [ ( 0 , "Expecting more input" ) ]
        ( head : tail ) ->
            let c = Char.toCode head
            in
                if c == 34 || c == 92 || c < 32 || c > 1114111
                then Err [ ( 0 , "Expecting a valid character" ) ]
                else Ok ( 1 , [ String.fromChar head ] , tail )


character :: Parser ( List String )
{-
    Parse a character.
-}
character =
    Parse.oneOf
        [ char
        , Parse.sequence [ Parse.string "\\" , escape ]
        ]


characters :: Parser ( List String )
{-
    Parse any number of characters.
-}
characters =
    Parse.zeroOrMore character


string :: Parser ( List String )
{-
    Parse a JSON string.
-}
string =
    Parse.sequence
        [ Parse.string "\""
        , characters
        , Parse.string "\""
        ]
    |> Parse.withError "Expecting a string"


-- Booleans and null ------------------------------------------------


bool :: Parser ( List String )
{-
    Parse `true` or `false`.
-}
bool =
    Parse.oneOf [ Parse.string "true" , Parse.string "false" ]
    |> Parse.withError "Expecting `true` or `false`"


null :: Parser ( List String )
{-
    Parse `null`.
-}
null =
    Parse.string "null"


-- JSON values ------------------------------------------------------


atom :: Parser Json
{-
    Parse an atomic JSON value into a `Json` value.
-}
atom =
    Parse.oneOf [ string , number , bool , null ]
    |> Parse.map ( List.foldr (++) [] >> Jatom )


value :: Parser Json
{-
    Parse a JSON value into a `Json` value.
-}
value =
    Parse.oneOf [ object , array , atom ]


-- JSON arrays ------------------------------------------------------


element :: Parser Json
{-
    Parse an element of a JSON array.
-}
element =
    Parse.sequence
        [ Parse.throwAway whitespace
        , Parse.map List.singleton value
        , Parse.throwAway whitespace
        ]
    >> \ case
        Err errs -> Err errs
        Ok ( n , [ v ] , pending ) -> Ok ( n , v , pending )
        Ok ( n , _ , _ ) -> Err [ ( n , "Expecting end of input" ) ]


elements :: Parser ( List Json )
{-
    Parse elements of a JSON array.
-}
elements =
    Parse.sequence
        [ Parse.map List.singleton element
        , Parse.optional
            ( Parse.sequence
                [ Parse.throwAway ( Parse.string "," )
                , elements
                ]
            )
        ]


array :: Parser Json
{-
    Parse a JSON array into a `Jarray`.
-}
array =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "[" )
        , Parse.oneOf [ elements , Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "]" )
        ]
    |> Parse.map Jarray


-- JSON objects -----------------------------------------------------


stringAtom :: Parser Json
{-
    Parse a JSON string into a `Json` value.

    This is a version of `atom` that only parses strings.
    We need this because the keys of JSON objects are only
    allowed to be strings.
-}
stringAtom =
    string
    |> Parse.map ( List.foldr (++) [] )
    |> Parse.map Jatom


member :: Parser ( Json , Json )
{-
    Parse a member of a JSON object.
-}
member =
    Parse.sequence
        [ Parse.throwAway whitespace
        , Parse.map List.singleton stringAtom
        , Parse.throwAway whitespace
        , Parse.throwAway ( Parse.string ":" )
        , Parse.map List.singleton element
        ]
    >> \ case
        Err errs -> Err errs
        Ok ( n , [ k , v ] , pending ) -> Ok ( n , ( k , v ) , pending )
        Ok ( n , _ , _ ) -> Err [ ( n , "Expecting an object member" ) ]


members :: Parser ( List ( Json , Json ) )
{-
    Parse the members of a JSON object.
-}
members =
    Parse.sequence
        [ Parse.map List.singleton member
        , Parse.optional
            ( Parse.sequence
                [ Parse.throwAway ( Parse.string "," )
                , members
                ]
            )
        ]


object :: Parser Json
{-
    Parse a JSON object.
-}
object =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "{" )
        , Parse.oneOf [ members , Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "}" )
        ]
    |> Parse.map Jobject


-- JSON document ----------------------------------------------------


json :: Parser Json
{-
    Parse an entire JSON document.
-}
json =
    element
    >> \ case
        Err errs -> Err errs
        Ok ( n , done , "" ) -> Ok ( n , done , "" )
        Ok ( n , _ , _ ) -> Err [ ( n , "Expecting end of input" ) ]
