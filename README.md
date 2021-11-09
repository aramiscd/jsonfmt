# jsonfmt

A JSON pretty printer.

Read a JSON document from stdin, reformat it and write it to stdout.
No configuration, no command line options.

![jsonfmt](jsonfmt.gif)

The formatter operates purely on the JSON syntax tree.  Whitespace
is not taken into consideration.  Commas are placed in a leading
position.

This program was an exercise in writing a combinatory parser based
on a DIY parsing library.  Thus, I'm not sure how well it handles
large inputs.  I do use it in practice to format JSON documents
though, and I have never run into any problems so far.


## Installation

### From source

```shell
$ git clone https://git.sr.ht/~aramis/jsonfmt
$ cd jsonfmt
$ cabal install
```

(Make sure `~/.cabal/bin/` is in your `$PATH`.)


## Usage

### Kakoune

```text
%|jsonfmt
```


### Vim/Neovim

```text
:%!jsonfmt
```
