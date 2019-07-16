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
    , throwAway

    , map
    , merge

    -- advanced parsers

    , whitespace
    , optWhite
    )
where
        
import Ulme hiding ( map )
import qualified Ulme.List as List
import qualified Ulme.String as String


type Parser a
    = String -> Result ( String, String ) ( a, String )


char :: Char -> Parser [ String ]
{-
Parse a single `Char`.
-}
char match input =
    let error = Err ( String.fromChar match, input ) in
    case input of
        head : tail ->
            if head == match
            then Ok ( [ String.fromChar match ], tail )
            else error 

        _ -> error


string :: String -> Parser [ String ]
string match input =
{-
Parse a `String`.
-}
    if String.startsWith match input
    then
        Ok
            ( [ match ]
            , String.dropLeft ( String.length match ) input
            )
    else
        Err
            ( match
            , input
            )


succeed :: a -> Parser a
{-
Don't parse anything, just succeed.

Useful as a `fold` kick-starter for the sequential
application of parsers.  We could directly use `Ok`
instead, but this is probably more readable.
-}
succeed value input =
    Ok ( value, input )


fail :: Parser a
{-
Don't parse anything, just fail.

Useful as a `fold` kick-starter for trying out parsers in
parallel until one succeeds.
-}
fail input =
    Err ( "", input )


end :: a -> Parser a
{-
Succeed on the empty string, fail otherwhise.

Useful as the last parser in a sequence of parsers to make
sure that there is no remaining input.
-}
end value input =
    if input == "" then Ok ( value, "" ) else Err ( "", input )


succ :: Parser [ a ] -> Parser [ a ] -> Parser [ a ]
{-
Apply two parsers, one after the other.
-}
succ parser1 parser2 input =
    case parser1 input of
        Ok ( done1, pending1 ) ->
            case parser2 pending1 of
                Ok ( done2, pending2 )  -> Ok ( done1 ++ done2, pending2 )
                error                   -> error

        error -> error


sequence :: [ Parser [ a ] ] -> Parser [ a ]
{-
Apply a list of parsers, one after another.
-}
sequence parsers =
    List.foldr succ ( succeed [] ) parsers


either :: Parser a -> Parser a -> Parser a
{-
Apply the first succeeding parser from two alternatives.
-}
either parser1 parser2 input =
    case parser1 input of
        Err _ -> parser2 input
        ok    -> ok


oneOf :: [ Parser a ] -> Parser a
{-
Apply the fist succeeding parser from a list of parsers.
-}
oneOf parsers =
    List.foldr either fail parsers


optional :: Parser [ a ] -> Parser [ a ]
{-
Optionally apply a parser.
-}
optional parse input =
    case parse input of
        Ok value -> Ok value
        _       -> Ok ( [], input )


oneOrMore :: Parser [ a ] -> Parser [ a ]
{-
Apply as often as possible, but at least once.
-}
oneOrMore parse =
    sequence [ parse, optional ( oneOrMore parse ) ]


zeroOrMore :: Parser [ a ] -> Parser [ a ]
{-
Apply a parser as often as possible.
-}
zeroOrMore parse =
    optional ( oneOrMore parse )


map :: ( a -> b ) -> Parser a -> Parser b
{-
Map over a parsing result.
-}
map fn parse input =
    case parse input of
        Ok ( done, pending )    -> Ok ( fn done, pending )
        Err error               -> Err error


merge :: Parser [ [ a ] ] -> Parser [ [ a ] ]
merge parse input =
    let do_merge = List.foldr (++) [] >> List.singleton in
    case parse input of
        Ok ( done, pending )    -> Ok ( do_merge done, pending )
        Err error               -> Err error


whitespace :: Parser [ String ]
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


optWhite :: Parser [ String ]
{-
Parse optional whitespace.
-}
optWhite =
    optional whitespace


nonZeroDigit :: Parser [ String ]
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


digit :: Parser [ String ]
digit =
    oneOf [ char '0', nonZeroDigit ]


throwAway :: Parser [ a ] -> Parser [ a ]
{-
Apply a parser and throw the result away.
-}
throwAway parse input =
    case parse input of
        Ok ( done, pending )    -> Ok ( [], pending )
        Err error               -> Err error
