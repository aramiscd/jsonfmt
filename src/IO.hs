module IO

{-
    `System.IO` is full of functions that might throw
    exceptions at runtime.  For example, `hGetContents`
    blows up when it gets bad data, `hPutStr` blows up
    when it is applied to a closed handle.

    I want these failures to be handled at the type
    level.  Hence this wrapper module!  It provides safe
    alternatives to some potentially failing `IO` functions
    in `System.IO`.

    I'm also renaming some things to get rid of single-
    letter prefixes.  `hGetContents` should really be
    named `getContents`, and the current `getContents`
    should be written as `getContents stdin`.

    This module just contains what I need.  It isn't an
    exhaustive alternative to `System.IO`.
-}

( Mode ( Read , Write , Append , ReadWrite )
, stdin
, stdout
, stderr
, print
, println
, printErr
, putStr
, putStrLn
, getContents
, openFile
)
where


import Ulme

import qualified Control.Exception.Safe
import qualified System.IO
import qualified System.IO.Strict

import Control.Exception.Safe   ( SomeException )
import Data.Either              ( Either ( Left , Right ) )
import System.IO                ( Handle )


data Mode
{-
    File access modes.
-}
    = Read
    | Write
    | Append
    | ReadWrite


toIOMode :: Mode -> System.IO.IOMode
{-
    Translate `Mode` to `System.IO.IOMode`.
-}
toIOMode mode =
    case mode of
        Read        -> System.IO.ReadMode
        Write       -> System.IO.WriteMode
        Append      -> System.IO.AppendMode
        ReadWrite   -> System.IO.ReadWriteMode


try :: IO a -> IO ( Result SomeException a )
{-
    Try to perform an IO action.

    This is just like `Control.Exception.try` but it
    returns a `Result` (`Ok` or `Err`) instead of an
    `Either` (`Right` or `Left`).

    I don't like `Either` because `Left` vs. `Right`
    isn't semantically helpful.  In most cases it's just
    confusing because you never know what is left and what
    is right.
-}
try action =
    Control.Exception.Safe.tryAny action
    |> andThen
        ( \ case
            Left exception  -> return ( Err exception )
            Right contents  -> return ( Ok contents )
        )


stdin :: Handle
{-
    The standard input stream.
-}
stdin =
    System.IO.stdin


stdout :: Handle
{-
    The standard output stream.
-}
stdout =
    System.IO.stdout


stderr :: Handle
{-
    The standard error output stream.
-}
stderr =
    System.IO.stderr


print :: String -> IO ()
{-
    Write to the standard output stream.
-}
print string =
    System.IO.putStr string


println :: String -> IO ()
{-
    Write to the standard output stream, appending
    a newline.
-}
println string =
    System.IO.putStrLn string


printErr :: String -> IO ()
{-
    Write to the standard error output stream, appending
    a newline.
-}
printErr string =
    System.IO.hPutStrLn stderr string


putStr :: Handle -> String -> IO ( Result SomeException () )
{-
    Write to a stream via its handle.

    This might fail with an `IOException`, for example when
    the stream has been closed already.  This function
    forces the programmer to handle the exception, while
    `System.IO.hPutStr` will blow up at runtime in this
    case.
-}
putStr handle string =
    try ( System.IO.hPutStr handle string )


putStrLn :: Handle -> String -> IO ( Result SomeException () )
{-
    Append a newline and write to a stream via its handle.

    This might fail with an `IOException`, for example when
    the stream has been closed already.  This function
    forces the programmer to handle the exception, while
    `System.IO.hPutStrLn` will blow up at runtime in
    this case.
-}
putStrLn handle string =
    try ( System.IO.hPutStrLn handle string )


getContents :: Handle -> IO ( Result SomeException String )
{-
    Get the contents of an input stream via its handle.

    This might fail with an `IOException`, for example
    when the stream contains bad data.  This function
    forces the programmer to handle the exception, while
    `System.IO.hGetContents` will blow up at runtime in
    this case.
-}
getContents handle =
    try ( System.IO.Strict.hGetContents handle )


openFile :: Mode -> String -> IO ( Result SomeException Handle )
{-
    Open a file in the given access mode.

    This might fail, for example when the file doesn't
    exist.  This function forces the programmer to handle
    the exception, while `System.IO.openFile` will blow
    up at runtime in this case.
-}
openFile mode path =
    try ( System.IO.openFile path ( toIOMode mode ) )
