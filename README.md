# jsonfmt

An experimental JSON pretty printer, inspired by
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

I'd like to provide a fully statically linked linux binary,
but I haven't yet figured out how create that from Haskell
sources.  For now there is just a provisional install
script for Ubuntu 18.04.  It will attempt to build jsonfmt
and copy it to `~/.local/bin/`.  It will not touch your
`$PATH`.

```bash
sudo apt install -y curl
```

```bash
curl -L https://git.sr.ht/~aramis/jsonfmt/blob/master/install.sh | sh
```


## Usage

In Vim/Neovim, type `:%!jsonfmt` and hit `Return` to format
a JSON document.

In Kakoune, type `%|jsonfmt` and hit `Return` to format
a JSON document.
