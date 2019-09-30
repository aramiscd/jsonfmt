module IO

{-
    I/O streams, I/O access, I/O functions.
-}

( Mode
    ( Read , Write , Append , ReadWrite )
, stdin
, stdout
, stderr

, print
, printLine
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
    I/O stream access modes.
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
-}
try action =
    Control.Exception.Safe.tryAny action
    |> andThen
        ( \ case
            Left error  -> return ( Err error )
            Right value -> return ( Ok value )
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
    Append a newline and write to the standard output
    stream.
-}
println string =
    System.IO.putStrLn string


printErr :: String -> IO ()
{-
    Append a newline and write to the standard error
    output stream.
-}
printErr string =
    System.IO.hPutStrLn stderr string


putStr :: Handle -> String -> IO ( Result SomeException () )
{-
    Write to a stream.
-}
putStr handle string =
    try ( System.IO.hPutStr handle string )


putStrLn :: Handle -> String -> IO ( Result SomeException () )
{-
    Append a newline and write to a stream.
-}
putStrLn handle string =
    try ( System.IO.hPutStrLn handle string )


getContents :: Handle -> IO ( Result SomeException String )
{-
    Get data from a stream.
-}
getContents handle =
    try ( System.IO.Strict.hGetContents handle )


openFile :: Mode -> String -> IO ( Result SomeException Handle )
{-
    Open a file.
-}
openFile mode path =
    try ( System.IO.openFile path ( toIOMode mode ) )
