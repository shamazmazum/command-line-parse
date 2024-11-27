# command-line-parse
[![CI tests](https://github.com/shamazmazum/command-line-parse/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/command-line-parse/actions/workflows/test.yml)

Yet another command line parsing utility for Common Lisp.

When studying various command line parsing tools for Common Lisp I was greatly
disappointed. The best such tool, in my opinion, is
[unix-opts](https://github.com/libre-man/unix-opts). However it has many flaws:

* It does not have support for subcommands
* It does not have support for arguments
* It is not purely functional and its parsers are not first-class Common Lisp
  objects (i.e. you define a parser with a macro `opts:define-opts` and have no
  ways to i.e. compose it with another parser).

This library addresses those limitations. Here it a little how-to in the form of
examples:

## Parsing of arguments

Let's define a parser:

``` lisp
(defparameter *parser*
  (seq
   (optional
    (flag   'foo
            :short #\f     :description "Foo mode")
    (option 'level "LEVEL"
            :long  "level" :description "Level of operation" :fn #'parse-integer))
   (argument 'input  "INPUT")
   (argument 'output "OUTPUT")))
```

This parser accepts an optional flag `-f`, an option `--level` (which must be a
number) and two positional arguments. You can derive a help message for the
user of your program from this parser:

~~~~
CL-USER> (command-line-parse:show-usage foo::*parser* "program")
"Usage: program [-f] [--level LEVEL] INPUT OUTPUT

Options and arguments description:
--level LEVEL     Level of operation
-f                Foo mode
"
~~~~

Parsing works as follows:

~~~~
CL-USER> (command-line-parse:parse foo::*parser* '("--level" "6" "aaa" "bbb"))
((FOO::OUTPUT . "bbb") (FOO::INPUT . "aaa") (FOO::LEVEL . 6))
~~~~

## Choice operator

Suppose you want either the flag `--foo` or `--bar` (but not two) to be
specified, along with one positional argument. You can write

``` lisp
(defparameter *parser*
  (seq
   (choice
    (flag 'foo
          :short #\f :description "Foo mode")
    (flag 'foo
          :short #\b :description "Bar mode"))
   (argument 'data "DATA")))
```

The help message produced with `show-usage` looks like

~~~~
Usage: program -f | -b DATA

Options and arguments description:
-b     Bar mode
-f     Foo mode
~~~~

This evaluates to completion:

~~~~
CL-USER> (command-line-parse:parse foo::*parser* '("-f" "bbb"))
((FOO::DATA . "bbb") (FOO::FOO . T))
~~~~

This signals:

~~~~
CL-USER> (command-line-parse:parse foo::*parser* '("-f" "-b" "bbb"))
; Evaluation aborted on #<COMMAND-LINE-PARSE:CMD-LINE-PARSE-ERROR "Unparsed input remians: ~s" {1126661363}>.
~~~~

## Subcommands

You can make different subcommands using `command` parser and the outermost
`choice`:

``` lisp
(defparameter *cmd1*
  (seq
   (command 'what "print" 'print)
   (optional
    (flag 'verbose
          :short #\v :long "verbose" :description "Foo mode"))
   (argument 'data "DATA")))

(defparameter *cmd2*
  (seq
   (command 'what "format" 'format)
   (optional
    (flag 'force
          :long "force" :description "Force formatting"))
   (argument 'disk "DISK")))

(defparameter *parser*
  (choice *cmd1* *cmd2*))
```

Parsing examples:

~~~~
CL-USER> (command-line-parse:parse foo::*parser* '("format" "--force" "bbb"))
((FOO::DISK . "bbb") (FOO::FORCE . T) (FOO::WHAT . FORMAT))
CL-USER> (command-line-parse:parse foo::*parser* '("print" "bbb"))
((FOO::DATA . "bbb") (FOO::WHAT . PRINT))
~~~~

## End of options sign

A sign `--` may serve as an indicator that there is no more options after it.

Define

``` lisp
(defparameter *parser*
  (seq
   (optional
    (flag 's :short #\s)
    (flag 'p :short #\p))
   (argument 'arg "ARG")))
```

Now compare

~~~~
CL-USER> (command-line-parse:parse foo::*parser* '("-s" "-p" "foo"))
((FOO::ARG . "foo") (FOO::P . T) (FOO::S . T))
CL-USER> (command-line-parse:parse foo::*parser* '("-s" "--" "-p"))
((FOO::ARG . "-p") (FOO::S . T))
~~~~

## List of parsers

* `flag` — Boolean flag
* `option` — Option
* `argument` — Positional argument
* `arguments` — Matched the whole rest of the input. Returns a list. May be
  useful for an arbitrary number of positional arguments.
* `command` — Matches a subcommand
* `choice` — Either `a` or `b` or `c` etc. Matches the first succeeding parser.
* `optional` — A wrapper for `option`s and `flag`s. Basically, repeats its
  children until no one succeeds.
* `seq` — Succeeds only if all its children succeed in a specified order.

One of typical parser orders is

``` lisp
(seq
 (optional
  ;; Flags or options
  )
 (argument ...)   ; Positional argument 1
 (argument ...)   ; Positional argument 2
 (arguments ...)) ; The rest of positional arguments
```

## Pure and impure functions

The functions which access the lisp process' argv array and output streams are
`parse-argv` and `print-usage`. They are portable and exploit UIOP's `(argv0)`
and `(command-line-arguments)`.

Their pure counterparts are `parse` and `show-usage`. The latter uses the
standard pretty printer, but wrapped in `with-output-to-string`, so I consider
it pure anyway.
