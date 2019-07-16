module Parse.Json where

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


json :: Parser [ String ]
json =
    element


_json :: Parser Json
_json =
    _element


value :: Parser [ String ]
value =
    Parse.oneOf
        [ object
        , array
        , string
        , number
        , Parse.string "true"
        , Parse.string "false"
        , Parse.string "null"
        ]


_value :: Parser Json
_value =
    Parse.oneOf
        [ _object
        , _array
        , _string
        , _number
        , _true
        , _false
        , _null
        ]


object :: Parser [ String ]
object =
    Parse.sequence
        [ Parse.char '{'
        , Parse.oneOf [ members, whitespace ]
        , Parse.char '}'
        ]


_object :: Parser Json
_object =
    Parse.sequence
        [ throwAwayPair ( Parse.char '{' )
        , Parse.oneOf [ _members, throwAwayPair whitespace ]
        , throwAwayPair ( Parse.char '}' )
        ]
    |> Parse.map Jobject


members :: Parser [ String ]
members =
    Parse.sequence
        [ member
        , Parse.optional
            ( Parse.sequence [ Parse.char ',', members ] )
        ]


_members :: Parser [ ( Json, Json ) ]
_members =
    Parse.sequence
        [ Parse.map List.singleton _member
        , Parse.optional
            ( Parse.sequence [ throwAwayPair ( Parse.char ',' ), _members ] )
        ]


member :: Parser [ String ]
member =
    Parse.sequence
        [ whitespace, string, whitespace, Parse.char ':', element ]


_member :: Parser ( Json, Json )
_member =
    Parse.sequence
        [ throwAwayList whitespace
        , Parse.map ( List.foldr (++) "" >> Jatom >> List.singleton ) string
        , throwAwayList whitespace
        , throwAwayList ( Parse.char ':' )
        , Parse.map List.singleton _element
        ]
    >> \ case
        Err error                   -> Err error
        Ok ( [ k, v ] , pending )   -> Ok ( ( k, v ), pending )
        Ok ( values, pending )      -> Err ( show values, pending )


array :: Parser [ String ]
array =
    Parse.sequence
        [ Parse.char '['
        , Parse.oneOf [ elements, whitespace ]
        , Parse.char ']'
        ]


_array :: Parser Json
_array =
    Parse.sequence
        [ throwAwayList ( Parse.char '[' )
        , Parse.oneOf [ _elements, throwAwayList whitespace ]
        , throwAwayList ( Parse.char ']' )
        ]
    |> Parse.map Jarray


elements :: Parser [ String ]
elements =
    Parse.sequence
        [ element
        , Parse.optional
            ( Parse.sequence [ Parse.char ',' , elements ] )
        ]


_elements :: Parser [ Json ]
_elements =
    Parse.sequence
        [ Parse.map List.singleton _element
        , Parse.optional
            ( Parse.sequence [ throwAwayList ( Parse.char ',' ), _elements ] )
        ]


element :: Parser [ String ]
element =
    Parse.sequence [ whitespace, value, whitespace ]


_element :: Parser Json
_element =
    Parse.sequence
        [ throwAwayList whitespace
        , Parse.map List.singleton _value
        , throwAwayList whitespace
        ]
    >> \ case
        Err error               -> Err error
        Ok ( [ v ] , pending )  -> Ok ( v, pending )
        Ok ( values, pending )  -> Err ( show values, pending )


string :: Parser [ String ]
string =
    Parse.sequence [ Parse.char '"', characters, Parse.char '"' ]
    |> Parse.merge


_string :: Parser Json
_string =
    Parse.sequence [ Parse.char '"', characters, Parse.char '"' ]
    |> Parse.map ( List.foldr (++) "" >> Jatom )


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
    Parse.oneOf [ char, Parse.sequence [ Parse.char '\\', escape ] ]


escape :: Parser [ String ]
escape =
    Parse.oneOf
        [ Parse.char '"'
        , Parse.char '\\'
        , Parse.char '/'
        , Parse.char 'b'
        , Parse.char 'f'
        , Parse.char 'n'
        , Parse.char 'r'
        , Parse.char 't'
        , Parse.sequence [ Parse.char 'u', hex, hex, hex, hex ]
        ]


hex :: Parser [ String ]
hex =
    Parse.oneOf
        [ digit
        , Parse.oneOf
            [ Parse.char 'A'
            , Parse.char 'B'
            , Parse.char 'C'
            , Parse.char 'D'
            , Parse.char 'E'
            , Parse.char 'F'
            , Parse.char 'a'
            , Parse.char 'b'
            , Parse.char 'c'
            , Parse.char 'd'
            , Parse.char 'e'
            , Parse.char 'f'
            ]
        ]


number :: Parser [ String ]
number =
    Parse.sequence [ integer, fraction, exponent ]
    |> Parse.merge


_number :: Parser Json
_number =
    Parse.sequence [ integer, fraction, exponent ]
    |> Parse.map ( List.foldr (++) "" >> Jatom )


integer :: Parser [ String ]
integer =
    Parse.oneOf
        [ Parse.sequence [ onenine, digits ]
        , digit
        , Parse.sequence [ Parse.char '-', onenine, digits ]
        , Parse.sequence [ Parse.char '-', digit ]
        ]


digits :: Parser [ String ]
digits =
    Parse.oneOrMore digit


digit :: Parser [ String ]
digit =
    Parse.oneOf [ Parse.char '0', onenine ] 


onenine :: Parser [ String ]
onenine =
    Parse.oneOf
        [ Parse.char '1'
        , Parse.char '2'
        , Parse.char '3'
        , Parse.char '4'
        , Parse.char '5'
        , Parse.char '6'
        , Parse.char '7'
        , Parse.char '8'
        , Parse.char '9'
        ]


fraction :: Parser [ String ]
fraction =
    Parse.optional
        ( Parse.sequence [ Parse.char '.', digits ] )


exponent :: Parser [ String ]
exponent =
    Parse.optional
        ( Parse.sequence
            [ Parse.oneOf [ Parse.char 'E', Parse.char 'e' ]
            , sign
            , digits
            ]
        )


sign :: Parser [ String ]
sign =
    Parse.optional
        ( Parse.oneOf [ Parse.char '+', Parse.char '-' ] )


whitespace :: Parser [ String ]
whitespace =
    Parse.zeroOrMore
        ( Parse.oneOf
            [ Parse.char '\x0020'
            , Parse.char '\x000D'
            , Parse.char '\x000A'
            , Parse.char '\x0009'
            ]
        )


_true :: Parser Json
_true =
    Parse.string "true"
    |> Parse.map ( List.foldr (++) [] >> Jatom )


_false :: Parser Json
_false =
    Parse.string "false"
    |> Parse.map ( List.foldr (++) [] >> Jatom )


_null :: Parser Json
_null =
    Parse.string "null"
    |> Parse.map ( List.foldr (++) [] >> Jatom )


throwAwayList :: Parser [ String ] -> Parser [ Json ]
throwAwayList =
    Parse.map ( \ _ -> [ Jatom "" ] )
    >> Parse.throwAway


throwAwayPair :: Parser [ String ] -> Parser [ ( Json, Json ) ]
throwAwayPair =
    Parse.map ( \ _ -> [ ( Jatom "", Jatom "" ) ] )
    >> Parse.throwAway
