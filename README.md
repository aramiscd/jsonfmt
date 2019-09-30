# jsonfmt

A simple JSON pretty printer, inspired by
[elm-format](https://github.com/avh4/elm-format), written
in Haskell.

It reads a JSON document from the standard input, reformats
it and writes it to the standard output.  It has no
configuration or command line options.

It will turn this:

```json
{"k1":"v1","k2":"v2","k3":["e1","e2","e3"]}
```
    
into this:

```json
{ "k1" : "v1"
, "k2" : "v2"
, "k3" :
    [ "e1"
    , "e2"
    , "e3"
    ]
}
```


## Installation

Download jsonfmt and put it in your `$PATH`.


## Usage

### Vim/Neovim

Type `:%!jsonfmt` and hit `return` to format a JSON
document.


### Kakoune

Type `%|jsonfmt` and hit `return` to format a JSON
document.
