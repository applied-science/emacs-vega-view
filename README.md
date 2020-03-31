# emacs-vega-view

This is a small library meant to facilitate exploratory data
visualization using [Vega](https://vega.github.io/vega/) from within
emacs.

## Installation

In order to use this package, you must have the Vega command line
tools on your path. The easiest way to arrange that is to install the
latest version with using `npm`:

```sh
npm install -g vega vega-lite vega-cli
```

## Usage

Vega-view currently supports a single interactive function,
`vega-view`, that can be invoked at the end of a form to visualize it
as a Vega plot. Currently, three kinds of Vega notation are supported:

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
`*vega*`. (Note that you can toggle between viewing an `SVG` image in
an `image-mode` buffer as image or text using `C-c C-c` in that
buffer.)

### `JSON`

Suppose you have a `json-mode` buffer containing this Vega specification:

``` json
{
    "data": {
        "values": [
            {"a": "A", "b": 28}, {"a": "B", "b": 55}, {"a": "C", "b": 43},
            {"a": "D", "b": 91}, {"a": "E", "b": 81}, {"a": "F", "b": 53},
            {"a": "G", "b": 19}, {"a": "H", "b": 87}, {"a": "I", "b": 52}
        ]
    },
    "mark": "bar",
    "encoding": {
        "x": {"field": "a", "type": "ordinal", "axis": {"labelAngle": 0}},
        "y": {"field": "b", "type": "quantitative"}
    }
}
```

Placing the cursor after the final `}` and invoking `vega-view` will
bring up a new window (in the emacs sense of the term) containing an
SVG drawing made from this spec.

![json example plot](https://raw.githubusercontent.com/appliedsciencestudio/emacs-vega-view/master/json-example.svg?sanitize=true)

### `elisp`

The code sample below was produced by invoking `pp-eval-last-sexp`
after `(json-read-file "sample.json")` in the `*scratch*` buffer. The
contents of `sample.json` are the same as in the `JSON` example above.

If we place the cursor after the final parenthesis of this `elisp`
form, it will show the same drawing that was generated above.

``` emacs-lisp
'(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
  (description . "A simple bar chart with embedded data.")
  (data
   (values . [((a . "A") (b . 28)) ((a . "B") (b . 55))
              ((a . "C") (b . 43)) ((a . "D") (b . 91))
              ((a . "E") (b . 81)) ((a . "F") (b . 53))
              ((a . "G") (b . 19)) ((a . "H") (b . 87))
              ((a . "I") (b . 52))]))
  (mark . "bar")
  (encoding
   (x (field . "a")
      (type . "ordinal")
      (axis (labelAngle . 0)))
   (y (field . "b")
      (type . "quantitative"))))
```

Note that, because the `elisp` code is evaluated before being sent to
Vega, one can produce programmatic graphs easily using only
`elisp`. For example, this code will plot a line chart of the `sin`
function:

``` emacs-lisp
(require 'seq)

`(($schema . "https://vega.github.io/schema/vega-lite/v4.json")
 (description . "A simple bar chart with embedded data.")
 (data
  (values . ,(seq-map-indexed (lambda (x i) `((a . ,i) (b . ,(sin x))))
                           '(0 1 2 3 4 5 6 7 8 9))))
 (mark . "line")
 (encoding
  (x (field . "a")
     (type . "ordinal") 
     (axis (labelAngle . 0)))
  (y (field . "b")
     (type . "quantitative"))))
```

![elisp example plot](https://raw.githubusercontent.com/appliedsciencestudio/emacs-vega-view/master/elisp-example.svg?sanitize=true)

### `clojure`

Just as in the case of `elisp`, one can write whatever `clojure` code
they prefer and see the result of evaluating it and passing it through
Vega (note that cider must be active!). For example, this form will
plot twenty random values as a line chart:

``` clojure
{:data {:values (map hash-map
                     (repeat :a)
                     (range 1 20)
                     (repeat :b)
                     (repeatedly #(* 100 (Math/random))))}
   :mark "bar",
   :width 800
   :height 600
   :encoding {:x {:field :a, :type "ordinal", :axis {"labelAngle" 0}},
              :y {:field :b, :type "quantitative"}}}
```

![clojure example plot](https://raw.githubusercontent.com/appliedsciencestudio/emacs-vega-view/master/clojure-example.svg?sanitize=true)

## TODO

The `vega-view` function should be split into two functions, one that
produces a drawing in a similar manner to `eval-preceding-sexp` (as
this `vega-view` does), and one that works like `eval-defun`
(converting the top level `sexp`).

Likewise, there should be an easy way to select between Vega-lite and
full Vega. Perhaps using the universal argument?
