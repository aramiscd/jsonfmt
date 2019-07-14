module Parse.Json ( json ) where

import Ulme

import Parse ( Parser )
import qualified Parse
import qualified Ulme.Char as Char
import qualified Ulme.List as List


json :: Parser
json =
    element


value :: Parser
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


object :: Parser
object =
    Parse.sequence
        [ Parse.char '{'
        , Parse.oneOf [ members, whitespace ]
        , Parse.char '}'
        ]


members :: Parser
members =
    Parse.sequence
        [ member
        , Parse.optional
            ( Parse.sequence [ Parse.char ',', members ] )
        ]


member :: Parser
member =
    Parse.sequence
        [ whitespace, string, whitespace, Parse.char ':', element ]


array :: Parser
array =
    Parse.sequence
        [ Parse.char '['
        , Parse.oneOf [ elements, whitespace ]
        , Parse.char ']'
        ]


elements :: Parser
elements =
    Parse.sequence
        [ element
        , Parse.optional
            ( Parse.sequence [ Parse.char ',' , elements ] )
        ]


element :: Parser
element =
    Parse.sequence [ whitespace, value, whitespace ]


string :: Parser
string =
    Parse.merge
        ( Parse.sequence [ Parse.char '"', characters, Parse.char '"' ] )


characters :: Parser
characters  =
    Parse.zeroOrMore character


character :: Parser
character =
    let
        codes
            = List.range 32 33
           ++ List.range 35 91
           ++ List.range 93 1114111

        chars =
            codes
            |> map Char.fromCode
            |> map Parse.char
    in
    Parse.oneOf
        [ Parse.oneOf chars
        , Parse.sequence [ Parse.char '\\', escape ]
        ]


escape :: Parser
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


hex :: Parser
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


number :: Parser
number =
    Parse.merge
        ( Parse.sequence [ integer, fraction, exponent ] )


integer :: Parser
integer =
    Parse.oneOf
        [ Parse.sequence [ onenine, digits ]
        , digit
        , Parse.sequence [ Parse.char '-', onenine, digits ]
        , Parse.sequence [ Parse.char '-', digit ]
        ]


digits :: Parser
digits =
    Parse.oneOrMore digit


digit :: Parser
digit =
    Parse.oneOf [ Parse.char '0', onenine ] 


onenine :: Parser
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


fraction :: Parser
fraction =
    Parse.optional
        ( Parse.sequence [ Parse.char '.', digits ] )


exponent :: Parser
exponent =
    Parse.optional
        ( Parse.sequence
            [ Parse.oneOf
                [ Parse.char 'E'
                , Parse.char 'e'
                ]
            , sign
            , digits
            ]
        )


sign :: Parser
sign =
    Parse.optional
        ( Parse.oneOf [ Parse.char '+', Parse.char '-' ] )


whitespace :: Parser
whitespace =
    Parse.zeroOrMore
        ( Parse.oneOf
            [ Parse.char '\x0020'
            , Parse.char '\x000D'
            , Parse.char '\x000A'
            , Parse.char '\x0009'
            ]
        )
