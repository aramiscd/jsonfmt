module Parse

{-
    A simple parser-combinator module.

    This work is part of my first attempt at non-trivial
    parsing.  It is motivated by this presentation:
    https://vimeo.com/171704565

    Parser-combinators are surprisingly accessible.

    First, we collect tokens as lists of strings using the
    `string` function and some basic parsers.  From these
    tokens we can construct other data using those same
    combinators and the `map` function, perhaps discarding
    tokens using the `throwAway` function.

    For practical reasons most functions in this module
    rely on lists.  We can, of course, parse into any data
    type, but once we leave the monadic context of a list,
    we lose the ability to assemble values from multiple
    tokens and to pull apart or throw away values.


    ----

    Copyright 2019, Aramis Concepcion Duran
    
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

import qualified Ulme.List      as List
import qualified Ulme.String    as String


type Parser a
{-
    A `Parser a` attempts to turn a `String` into a
    value of type `a`.

    If it fails, it results in an `Err error` where `error`
    is a `ParseError`.  If it succeeds, it results in an
    `Ok ( n , v , s )` where `n` is the number of consumed
    characters, `v` is a value of type `a` and `s` is the
    remaining input.
-}
    = String -> Result ParseError ( Int , a , String )


type ParseError
{-
    A parse error type.
-}
    = String


string :: String -> Parser [ String ]
{-
    Parse a `String`.
-}
string match input =
    if
        String.startsWith match input
    then
        Ok
            ( String.length match
            , [ match ]
            , String.dropLeft ( String.length match ) input
            )
    else
        Err "" -- todo


optional :: Parser [ a ] -> Parser [ a ]
{-
    Optionally apply a parser.
-}
optional parse input =
    case parse input of
        Err _       -> Ok ( 0 , [] , input )
        Ok value    -> Ok value


throwAway :: Parser [ a ] -> Parser [ b ]
{-
    Apply a parser and throw away the result.
-}
throwAway =
    map ( always [] )
    >> \ parse input ->
        case parse input of
            Err error               -> Err error
            Ok ( n , _ , pending )  -> Ok ( n , [] , pending )


succeed :: a -> Parser a
{-
    Don't consume any input, just succeed with a value.

    Useful as a `fold` kick-starter for the sequential
    application of parsers.
-}
succeed value input =
    Ok ( 0 , value , input )


fail :: Parser a
{-
    Don't consume any input, just fail.

    Useful as a `fold` kick-starter for trying out parsers
    in parallel until one succeeds.
-}
fail _input =
    Err "" -- todo


zeroOrMore :: Parser [ a ] -> Parser [ a ]
{-
    Apply a parser as often as possible.
-}
zeroOrMore parse =
    optional ( oneOrMore parse )


oneOrMore :: Parser [ a ] -> Parser [ a ]
{-
    Apply a parser as often as possible but at least once.
-}
oneOrMore parse =
    sequence [ parse , optional ( oneOrMore parse ) ]


either :: Parser a -> Parser a -> Parser a
{-
    Apply the first succeeding parser from two
    alternatives.
-}
either parser1 parser2 input =
    case parser1 input of
        Err _       -> parser2 input
        Ok value    -> Ok value


oneOf :: [ Parser a ] -> Parser a
{-
    Apply the fist succeeding parser from a list of
    parsers.
-}
oneOf parsers =
    List.foldr either fail parsers


succ :: Parser [ a ] -> Parser [ a ] -> Parser [ a ]
{-
    Apply two parsers one after the other.
-}
succ parser1 parser2 input =
    case parser1 input of
        Err error -> Err error
        Ok ( n1 , done1 , pending1 ) ->
            case parser2 pending1 of
                Err error -> Err error
                Ok ( n2 , done2 , pending2 ) ->
                    Ok ( n1 + n2 , done1 ++ done2 , pending2 )


sequence :: [ Parser [ a ] ] -> Parser [ a ]
{-
    Apply a list of parsers, one after another.
-}
sequence parsers =
    List.foldr succ ( succeed [] ) parsers


map :: ( a -> b ) -> Parser a -> Parser b
{-
    Map over the result of a successful parser.
-}
map fn parse input =
    case parse input of
        Err error                   -> Err error
        Ok ( n , done , pending )   -> Ok ( n , fn done , pending )
