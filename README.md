# jsonfmt

**Work in progress!**

An experimental JSON pretty printer, inspired by
[elm-format](https://github.com/avh4/elm-format), written
in Haskell.

It reads a JSON document from the standard input, reformats
it and writes it to the standard output.  It has no
configuration or command line options.

It will turn this:

```json
{
  "key_1": "value_1",
  "key_2": "value_2",
  "key_3": ["elem_1","elem_2","elem_3"]
}
```
    
into this:

```json
{ "key_1" : "value_1"
, "key_2" : "value_2"
, "key_3" :
    [ "elem_1"
    , "elem_2"
    , "elem_3"
    ]
}
```

Right now this program is mainly an artifact demontrating
that I can write a simple parser.  I do use it in practice
to format JSON documents, but it can only handle relatively
small inputs.


## Installation

### From source

```shell
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.5
    
$ cabal --version
cabal-install version 3.0.0.0
compiled using version 3.0.0.0 of the Cabal library
```

```shell
$ git clone https://git.sr.ht/~aramis/jsonfmt
$ cd jsonfmt
$ cabal v2-install
```

Make sure `~/.cabal/bin/` is in your `$PATH`.


## Usage

### Kakoune

```text
%|jsonfmt
```


### Vim/Neovim

```text
:%!jsonfmt
```
