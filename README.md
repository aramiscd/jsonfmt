# jsonfmt

A simple JSON pretty-printer, inspired by
[elm-format](https://github.com/avh4/elm-format).

Distinguishing features:

* Arrays and objects are formatted like this:

  ```json
  [ "element_1"
  , "element_2"
  , "element_3"
  ]
  
  { "key_1" : "value_1"
  , "key_2" : "value_2"
  , "key_3" : "value_3"
  }
  ```
    
  instead of this:

  ```json
  [ "element_1",
    "element_2",
    "element_3"
  ]
  
  { "key_1" : "value_1",
    "key_2" : "value_2",
    "key_3" : "value_3"
  }
  ```

* Input whitespace isn't considered.  No matter how you
  wrap your JSON, the output stays the same.

* No command line options and flags.
  (I should perhaps add `-h, --help`.)

* It's written in Haskell.

----

The prefix-comma style for array elements and object
members is taken from [Elm](https://elm-lang.org).  I like
this style a lot.  It is diff-friendly and makes commas
more visible.

Also, after a long period of failure and despair in this
matter, I finally figured out how to write parsers, which
brings my yak-shaving skills to the next level.  The first
touchstone that came to mind was to write a JSON parser
and pretty-printer.


## Editor Integration

A great property of Vim-like text editors is that you don't
actually need plugins for things like code formatting.
Any program that reads from stdin and prints to stdout can
be invoked to operate on the contents of a text buffer.
Editor "integration" is merely a matter of binding this
action to a key.  I like to bind code formatters to the
tab key.


### Neovim

Most of this is trying to keep the cursor position intact
(because Vim doesn't on its own).  The actual binding is
done by `nnoremap`.

```
" ~/.config/nvim/after/ftplugin/json.vim

function! Fmt()
  execute "normal! mz"
  try
    undojoin
    silent execute "%!jsonfmt"
  catch /^Vim\%((\a\+)\)\=:E790/
  endtry
  try
    execute "normal! `z"
  catch /E20:/
    execute "normal! G"
  endtry
endfunction

nnoremap <silent> <tab> :call Fmt()<cr>
setlocal shiftwidth=2
setlocal tabstop=2
```


### Kakoune

This allows you to set filetype-specific options in a file
under `~/.config/kak/filetype/`:

```
# ~/.config/kak/kakrc

hook global WinSetOption filetype=.* %{
  try %{ source %sh{
    echo ~/.config/kak/filetype/${kak_opt_filetype}.kak
}}}
```

Haskell options go into `haskell.kak`, JavaScript options
go into `javascript.kak`, and so on.  To see what filetype
is associated with the current buffer, do this:

```
:echo %opt{filetype}
```

Finally, set the code formatter for JSON to `jsonfmt`:

```
# ~/.config/kak/filetype/json.kak

set window formatcmd 'jsonfmt'
```

As long as you stick to the `window` scope, these settings
won't affect other buffers and filetypes.
