# jsonfmt

Ein JSON Pretty-Printer

Liest ein JSON-Dokument von der Standardeingabe, formatiert es und
schreibt es auf die Standardausgabe.  Keine Konfiguration, keine
Kommandozeilenargumente.

![jsonfmt](jsonfmt.gif)

Die Formatierung operiert ausschließlich auf dem JSON-Syntaxbaum.
Whitespace (Zeilenumbrüche etc.) spielt keine Rolle.  Kommata werden
am Anfang platziert.

Der Code basiert auf einer kleinen Bibliothek für kombinatorisches
Parsen, die ich selbst geschrieben habe.  Ich weiß nicht, wie gut die
Anwendung mit großen Dateien zurechtkommt.  Ich nutze sie regelmäßig,
um JSON-Dokumente zu formatieren und hatte bisher keine Probleme.


## Installation

Einfach kompilieren, als ausführbar markieren und in ein `$PATH`
Verzeichnis verschieben, bspw.:

```shell
$ git clone https://git.sr.ht/~aramis/jsonfmt
$ cd jsonfmt
$ gren make src/Main.gren --output=jsonfmt
$ chmod +x jsonfmt
$ mv jsonfmt ~/.local/bin/
```


## Verwendung

Jeder Editor, der Text durch eine externe Anwendung pipen kann, kann
`jsonfmt` verwenden.


### Helix, Kakoune

```text
%|jsonfmt
```


### Vim/Neovim

```text
:%!jsonfmt
```
