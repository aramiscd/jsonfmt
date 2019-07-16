module Parse
    ( Parser
    , string
    , optional
    , throwAway
    , zeroOrMore
    , oneOrMore
    , oneOf
    , sequence
    , map
    )
where
        
import Ulme hiding ( map )
import qualified Ulme.List as List
import qualified Ulme.String as String


type Parser a
    = String -> Result ( String, String ) ( a, String )


string :: String -> Parser [ String ]
{-
    Parse a `String`.
-}
string match input =
    if String.startsWith match input
    then
        Ok
            ( [ match ]
            , String.dropLeft ( String.length match ) input
            )
    else
        Err ( match, input )


optional :: Parser [ a ] -> Parser [ a ]
{-
    Optionally apply a parser.
-}
optional parse input =
    case parse input of
        Ok value -> Ok value
        _        -> Ok ( [], input )


throwAway :: Parser [ String ] -> Parser [ a ]
{-
    Apply a parser and throw the result away.
-}
throwAway =
    map ( always [] )
    >> \ parse input ->
        case parse input of
            Ok ( done, pending ) -> Ok ( [], pending )
            Err error            -> Err error


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

    Useful as a `fold` kick-starter for trying out parsers
    in parallel until one succeeds.
-}
fail input =
    Err ( "", input )


zeroOrMore :: Parser [ a ] -> Parser [ a ]
{-
    Apply a parser as often as possible.
-}
zeroOrMore parse =
    optional ( oneOrMore parse )


oneOrMore :: Parser [ a ] -> Parser [ a ]
{-
    Apply as often as possible, but at least once.
-}
oneOrMore parse =
    sequence [ parse, optional ( oneOrMore parse ) ]


either :: Parser a -> Parser a -> Parser a
{-
    Apply the first succeeding parser from two
    alternatives.
-}
either parser1 parser2 input =
    case parser1 input of
        Err _ -> parser2 input
        ok    -> ok


oneOf :: [ Parser a ] -> Parser a
{-
    Apply the fist succeeding parser from a list of
    parsers.
-}
oneOf parsers =
    List.foldr either fail parsers


succ :: Parser [ a ] -> Parser [ a ] -> Parser [ a ]
{-
    Apply two parsers, one after the other.
-}
succ parser1 parser2 input =
    case parser1 input of
        Ok ( done1, pending1 ) ->
            case parser2 pending1 of
                Ok ( done2, pending2 ) ->
                    Ok ( done1 ++ done2, pending2 )
                Err error -> Err error
        error -> error


sequence :: [ Parser [ a ] ] -> Parser [ a ]
{-
    Apply a list of parsers, one after another.
-}
sequence parsers =
    List.foldr succ ( succeed [] ) parsers


map :: ( a -> b ) -> Parser a -> Parser b
{-
    Map over a parsing result.
-}
map fn parse input =
    case parse input of
        Ok ( done, pending ) -> Ok ( fn done, pending )
        Err error            -> Err error
