## Major code examples for *Land of Lisp*<sup>[CLiki](https://cliki.net/Land%20of%20Lisp)</sup>

(For brief description of each, see the official Common Lisp [one](http://landoflisp.com/source.html).)

Chapter | Racket | Common Lisp + [ergolib](https://github.com/rongarret/ergolib)
--------|----------|----------
2 | [guess.rkt](guess.rkt) |
5 & 6 | [wizards-game.rkt](wizards-game.rkt) | [wizards-game.lisp](wizards-game.lisp)
7 | [graph-util.rkt](graph-util.rkt) | [graph-util.lisp](graph-util.lisp)
8, 9' | [wumpus.rkt](wumpus.rkt) | [wumpus.lisp](wumpus.lisp)
9 | | [orc-battle.lisp](orc-battle.lisp)
10 | | [evolution.lisp](evolution.lisp)
11 | | [robots.lisp](robots.lisp)
13 | | [webserver.lisp](webserver.lisp)
15 | | [dice-of-doom-v1.lisp](dice-of-doom-v1.lisp)

When the book instructs installing CLISP, the first 4 sections of this pictorial [tutorial](https://docs.racket-lang.org/quick/index.html) was helpful for familiarizing myself with DrRacket (IDE). To install Racket, simply `apt-get install racket` was enough (for linux).

## Equivalents table of keywords in order of appearance (only through ch.9)

Chapters | Common Lisp | Racket 7.2 | ergolib
--|------------|------------|-----------
~4 | `defparameter`, `defun` | `define` (for both) | `defv`, `defc`, `defun`
.| `ash` | `arithmetic-shift`
.| `setf` (supports *generalized reference*<sup>ch.9</sup>) | `set!` (does not)
.| `1-`, `1+` | `sub1`, `add1`
.| `flet` | [`let`](https://docs.racket-lang.org/guide/let.html#%28part._.Parallel_.Binding__let%29) with [`lambda`](https://docs.racket-lang.org/quick/index.html#%28part._.Functions_are_.Values%29) | `bb :fn`
.| `labels` | `letrec` | `bb :fn`
.| `princ` | `display`
.| Empty list `'()` is false | It's `#t`; there are `empty` (= `'()` = `null`) and `empty?`
.| `eq` | `eq?`
.| `oddp` | `odd?`
.| `progn` | `begin`
.| Last branch of `cond` starts with `t` | It starts with `else` | Last branch of `mcond` only has body
.| Each `case` branch is in `(` `)` | It's in `[` `]` ([grouping](https://docs.racket-lang.org/reference/case.html) cases is also possible)
.| `otherwise` starts the last branch of `case` | `else`
.| `find-if` (for any sequence) | `findf` (for list)
.| `eql` | `eqv?`
.| `equalp` | ø
.| `string-equal` | `string=?`
5 | `mapcar` (for list) | `map` (for list)
.| `remove-if-not` | `filter` | `filter`
.| `find` | ø ([how](https://docs.racket-lang.org/guide/lambda.html#%28part._lambda-keywords%29) to declare keyword arguments)
.| `push` (via `setf`) | ø (can be implemented with [macro](https://docs.racket-lang.org/quick/index.html#%28part._.Macros%29))
6 | `print` (preceded by a newline and followed by a space) | `writeln` (followed by a newline)
.| `prin1` | `write`
.| `loop` | function calling itself at the end, or [named](https://docs.racket-lang.org/guide/let.html#%28part._.Named_let%29) `let` | `iterate`
.| `read-from-string` | ø (can be implemented with [`open-input-string`](https://docs.racket-lang.org/reference/stringport.html#%28def._%28%28quote._~23~25kernel%29._open-input-string%29%29))
.| `concatenate 'string` | `string-append` | `cat` (for any sequence)
.| `string-trim` (matches characters individually) | `string-trim` (matches the string literally; require racket/string)
.| `prin1-to-string` | `~a`
.| `coerce 'string`, `coerce 'list` | `list->string`, `string->list`
.| `fresh-line` (ignores duplicate) | `newline`
7 | `substitute-if` | ø
.| `complement` | ø
.| `alphanumericp` | ø (`char-numeric?`, `char-alphabetic?`) 
.| `digit-char-p` | `char-numeric?`
.| `length` (for any sequence) | `length` (for list), `string-length` (for string), `sequence-length`
.| `subseq` (for any sequence) | `substring`, `vector-copy`, `subsequence` (for any sequence; requires data/collection) | `slice` (for any sequence)
.| `mapc` | `for-each` | `walk`
.| `with-open-file (:direction :output` | [`call-with-output-file`](https://docs.racket-lang.org/reference/file-ports.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._call-with-output-file%29%29), [`with-output-to-file`](https://docs.racket-lang.org/reference/file-ports.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._with-output-to-file%29%29) (with *thunk*)
.| `ext:shell` | `system`
.| `maplist` | ø |`mapcdr`
8 | `load` | `require` (with [`provide`](https://docs.racket-lang.org/quick/index.html#%28part._.Modules%29) in the referred file)
.| `loop repeat collect` | [`for/list`](https://docs.racket-lang.org/guide/for.html)
.| `loop from to` | `for/list` with `range` | `for … in (counter`
.| `set-difference` | `set-subtract` (require racket/set)
.| `remove-duplicates` (using `eql` by default) | *same* (using `equal?` by default)
.| `intersection` | `set-intersect` (require racket/set) | `intersection` (for sets)
.| `some` (for any sequence) | `ormap` (for list), `sequence-ormap`, [`for/or`](https://docs.racket-lang.org/reference/for.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._for%2For%29%29) (for any sequence)
.| `mapcan` | `append-map` | `mappend!`
.| `pushnew` | ø
9 | `make-array` | `make-vector`
.| `aref` | `vector-ref` | `ref` (for anything)
.| `setf` with *accessor* (via [`defsetf`](http://www.lispworks.com/documentation/HyperSpec/Body/m_defset.htm)??) | ø (specific *setters*)
.| `setf (aref` | `vector-set!`
.| `nth` | `list-ref` | `ref`
.| `make-hash-table` | `make-hash` | `make-dictionary`
.| `gethash` | `hash-ref` (throws error when key not found), `hash-ref … #f` (optional *failure-result* at the end), hash-has-key?` | `ref`
.| `setf (gethash` | `hash-set!`
.| `round` (returns with remainder—2 return values) | *same* (without remainder)
.| Second return value is ignored | It's not ignored
.| `multiple-value-bind` | [`call-with-values`](https://docs.racket-lang.org/reference/values.html#%28def._%28%28quote._~23~25kernel%29._call-with-values%29%29) (how to use only the 1st value??) | `bb :mv`
.| `dotimes` | `for`
.| `defstruct` | `struct` (immutable and opaque by default—there are [keywords](https://docs.racket-lang.org/guide/define-struct.html#%28part._struct-options%29) `#:mutable` and `#:prefab`; couldn't find a way to instantiate by naming properties)
.| `count` (for any sequence) | `sequence-count`
.| `position` (for any sequence) | `index-of` (for list)
.| `every` (for any sequence) | `andmap` (for list), `sequence-andmap`, `for/and`
.| `reduce` (for any sequence) | `foldl` ("from left"; for list), `sequence-fold` (without "l"), `for/fold`
.| `map` (for any sequence) | `sequence-map`
.| `sort` (for any sequence) | `sort` (for list), `vector-sort`
.| `functionp` | `procedure?`
.| `defmethod` (for type dispatching) | ø | `define-method`
.| `funcall` | ø
.| `incf`, `decf` (via `setf`) | ø
.| `struct` with dynamic default property value | ø 
.| `type-of` | `ø` (`<struct-name>?`)
.| inheritance of `struct` | [*structure subtype*](https://docs.racket-lang.org/guide/define-struct.html#%28part._struct-subtypes%29)

