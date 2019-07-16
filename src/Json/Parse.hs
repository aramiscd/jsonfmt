module Json.Parse where

import Ulme
import qualified Ulme.Char as Char
import qualified Ulme.List as List
import qualified Ulme.String as String

import Parse ( Parser )
import qualified Parse


data Json
    = Jatom String
    | Jarray [ Json ]
    | Jobject [ ( Json, Json ) ]
    deriving Show


json :: Parser Json
json =
    element


value :: Parser Json
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
jatom =
    Parse.map
        ( List.foldr (++) [] >> Jatom )


object :: Parser Json
object =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "{" )
        , Parse.oneOf [ members, Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "}" )
        ]
    |> Parse.map Jobject


members :: Parser [ ( Json, Json ) ]
members =
    Parse.sequence
        [ Parse.map List.singleton member
        , Parse.optional <|
            Parse.sequence
                [ Parse.throwAway ( Parse.string "," ), members ]
        ]


member :: Parser ( Json, Json )
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
array =
    Parse.sequence
        [ Parse.throwAway ( Parse.string "[" )
        , Parse.oneOf [ elements, Parse.throwAway whitespace ]
        , Parse.throwAway ( Parse.string "]" )
        ]
    |> Parse.map Jarray


elements :: Parser [ Json ]
elements =
    Parse.sequence
        [ Parse.map List.singleton element
        , Parse.optional <|
            Parse.sequence
                [ Parse.throwAway ( Parse.string "," ), elements ]
        ]


element :: Parser Json
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
string =
    jatom <|
        Parse.sequence
            [ Parse.string "\"", characters, Parse.string "\"" ]


characters :: Parser [ String ]
characters  =
    Parse.zeroOrMore character


char :: Parser [ String ]
char input =
    let error = Err ( "Expect valid char", input ) in
    case input of
        ( head : tail ) ->
            let code = Char.toCode head in
            if code == 34 || code == 92 || code < 32 || code > 1114111
            then error else Ok ( [ String.fromChar head ], tail )
        _ -> error


character :: Parser [ String ]
character =
    Parse.oneOf
        [ char, Parse.sequence [ Parse.string "\\", escape ] ]


escape :: Parser [ String ]
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
number =
    jatom <|
        Parse.sequence
            [ integer, fraction, exponent ]


integer :: Parser [ String ]
integer =
    Parse.oneOf
        [ Parse.sequence [ onenine, digits ]
        , digit
        , Parse.sequence [ Parse.string "-", onenine, digits ]
        , Parse.sequence [ Parse.string "-", digit ]
        ]


digits :: Parser [ String ]
digits =
    Parse.oneOrMore digit


digit :: Parser [ String ]
digit =
    Parse.oneOf [ Parse.string "0", onenine ] 


onenine :: Parser [ String ]
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
fraction =
    Parse.optional <|
        Parse.sequence
            [ Parse.string ".", digits ]


exponent :: Parser [ String ]
exponent =
    Parse.optional <|
        Parse.sequence
            [ Parse.oneOf [ Parse.string "E", Parse.string "e" ]
            , sign
            , digits
            ]


sign :: Parser [ String ]
sign =
    Parse.optional <|
        Parse.oneOf
            [ Parse.string "+", Parse.string "-" ]


whitespace :: Parser [ String ]
whitespace =
    Parse.zeroOrMore <|
        Parse.oneOf
            [ Parse.string "\x0020"
            , Parse.string "\x000D"
            , Parse.string "\x000A"
            , Parse.string "\x0009"
            ]
