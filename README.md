# emacs-vega-view

This is a small library meant to facilitate exploratory data
visualization using [Vega](https://vega.github.io/vega/) from within
emacs.

## Installation

In order to use this package, you must have the Vega command line
tools on your path. The easiest way to arrange that is to install the
latest version with using `npm`:

```sh
npm install -g vega-lite
npm install -g vega-cli
```

## Usage

Vega-view currently supports a single interactive function,
`vega-view`, that can be invoked at the end of a form to visualize it
as a Vega graph. Currently, three kinds of Vega notation are
supported:

* `JSON`, which is passed directly to Vega.
* `elisp`, which is evaluated and converted to `JSON` before being
  passed to Vega. The `elisp` Vega specification format is, not
  coincidentally, the same as what is produced by called `read-json`
  on any Vega `JSON` specification.
* `clojure`, which is evaluated in the buffer's current
  [cider](https://github.com/clojure-emacs/cider) context and
  converted to `JSON` before being passed to Vega. The `clojure`
  specification format is whatever `EDN` would translate into the
  `JSON` specification you want. This is, also not coincidentally, the
  same format one would use with
  [Oz](https://github.com/metasoarous/oz).

When `vega-view` is invoked it first identifies the preceding `sexp`
(whatever that means for the language of the buffer), performs the
mode-specific conversion described above, then pipes it through the
Vega command line tools to convert the specification to an `SVG`
drawing. The drawing -- or the errors produced by Vega while trying to
produce it -- are then displayed in an `image-mode` buffer called
`*vega*`. (N.B. you can toggle between viewing an `SVG` image in an
`image-mode` buffer as image or text using `C-c C-c` in that buffer.)

