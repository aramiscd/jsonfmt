module Parse
    ( Parser

    -- simple parsers

    , char
    , string
    , end

    -- combinators

    , oneOf
    , sequence

    , optional
    , zeroOrMore
    , oneOrMore

    -- advanced parsers

    , whitespace
    , optWhite
    )
where
        
import Ulme
import qualified Ulme.List as List
import qualified Ulme.String as String


type Parser =
    String -> Result ( String, String ) String


char :: Char -> Parser
{-
Parse a single `Char`.
-}
char match input =
    let error = Err ( String.fromChar match, input ) in
    case input of
        head : tail -> if head == match then Ok tail else error 
        _           -> error


string :: String -> Parser
string match input =
{-
Parse a `String`.
-}
    if String.startsWith match input
    then Ok ( String.dropLeft ( String.length match ) input )
    else Err ( match, input )


succeed :: Parser
{-
Don't parse anything, just succeed.

Useful as a `fold` kick-starter for the sequential
application of parsers.  We could directly use `Ok`
instead, but this is probably more readable.
-}
succeed input =
    Ok input


{-
Don't parse anything, just fail.

Useful as a `fold` kick-starter for trying out parsers in
parallel until one succeeds.
-}
fail :: Parser
fail input =
    Err ( "", input )


end :: Parser
{-
Succeed on the empty string, fail otherwhise.

Useful as the last parser in a sequence of parsers to make
sure that there is no remaining input.
-}
end input =
    if input == "" then Ok "" else Err ( "", input )


succ :: Parser -> Parser -> Parser
{-
Apply two parsers, one after the other.
-}
succ parser1 parser2 input =
    case parser1 input of
        Ok tail -> parser2 tail
        error   -> error


sequence :: [ Parser ] -> Parser
{-
Apply a list of parsers, one after another.
-}
sequence parsers =
    List.foldr succ succeed parsers


either :: Parser -> Parser -> Parser
{-
Apply the first succeeding parser from two alternatives.
-}
either parser1 parser2 input =
    case parser1 input of
        Err _ -> parser2 input
        ok    -> ok


oneOf :: [ Parser ] -> Parser
{-
Apply the fist succeeding parser from a list of parsers.
-}
oneOf parsers =
    List.foldr either fail parsers


optional :: Parser -> Parser
{-
Optionally apply a parser.
-}
optional parse input =
    case parse input of
        Ok tail -> Ok tail
        _       -> Ok input


oneOrMore :: Parser -> Parser
{-
Apply as often as possible, but at least once.
-}
oneOrMore parse =
    sequence [ parse, optional ( oneOrMore parse ) ]


zeroOrMore :: Parser -> Parser
{-
Apply a parser as often as possible.
-}
zeroOrMore parse =
    optional ( oneOrMore parse )


whitespace :: Parser
{-
Parse whitespace.
-}
whitespace =
    oneOrMore
        ( oneOf
            [ char ' '
            , char '\t'
            , char '\n'
            , char '\r'
            ]
        )


optWhite :: Parser
{-
Parse optional whitespace.
-}
optWhite =
    optional whitespace


nonZeroDigit :: Parser
{-
Parse a non-zero decimal digit 1-9.
-}
nonZeroDigit =
    oneOf
        [ char '1'
        , char '2'
        , char '3'
        , char '4'
        , char '5'
        , char '6'
        , char '7'
        , char '8'
        , char '9'
        ]


digit :: Parser
digit =
    oneOf [ char '0', nonZeroDigit ]
