---
title: shelltestrunner: command-line testing
---

<div id=title>
<h1>Command-line testing</h1>
<img src="site/title2.png">
</div>

* toc

*Human or Replicant ?*
If you have a testing situation such as this<sup><small>1</small></sup>,
we at Joyful Corp. can help.

**shelltestrunner** is a command-line tool for testing other command-line
programs, or general shell commands, on GNU/Linux, Mac and Windows.

It reads tests which specify a command to run, some input, and the
expected stdout, stderr, and exit status output.  It can run tests in
parallel, selectively, with a timeout, with color output, or with
expected/actual differences highlighted.  shelltestrunner is free
software released under GPLv3+.

<a name="note1">
<small><sup>1</sup> expressed on the command line</small>
</a>


## Getting started

---------------------------------|--------------------------------------
Debian,&nbsp;Ubuntu:&nbsp;&nbsp; | **`apt-get install shelltestrunner`**
Gentoo:                          | **`emerge shelltestrunner`**
Elsewhere:<br><br><br>           | Get [GHC](http://haskell.org/ghc) and cabal (or the [Haskell Platform](http://haskell.org/platform)),<br>ensure `~/.cabal/bin` is in your $PATH,<br>**`cabal install shelltestrunner`**

Tests are kept in files with the `.test` suffix by default. Here's a simple test file:

```bash
# echo, given no input, prints nothing and terminates normally
echo
>>>= 0
```

and another, containing two more tests:

```bash
# cat copies its input to stdout, nothing appears on stderr, exit status is 0
cat
<<<
foo
>>>
foo
>>>2
>>>= 0

# cat prints an error containing "unrecognized option" if given a bad flag
cat --no-such-flag
>>>2 /unrecognized option/
>>>= !0
```

Run the tests:

```bash
$ shelltest echo.test cat.test
:echo.test: [OK]
:cat.test:1: [OK]
:cat.test:2: [OK]

         Test Cases  Total      
 Passed  3           3          
 Failed  0           0          
 Total   3           3          
```

That's it!

## Test format

<!-- Two formats are supported: -->

<!-- ### Old format -->

Test files contain one or more tests, which look like this:
```bash
# optional comment
a one-line shell command
<<<
zero or more lines of standard input
>>>
zero or more lines of expected standard output (or /REGEXP/ added to the previous line)
>>>2
zero or more lines of expected standard error output (or /REGEXP/ added to the previous line)
>>>= STATUS (or /REGEXP/)
```

The command and the final exit status line are required; the other parts are optional.

A `/REGEXP/` pattern may be used instead of specifying the full
output, in which case a match anywhere in the output allows the test
to pass. The regular expression syntax is
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa)'s, plus
you can put `!` before `/REGEXP/` to negate the match.

`STATUS` is a numeric [exit status](http://en.wikipedia.org/wiki/Exit_status)
or a `/REGEXP/`. Again, use a `!` prefix to negate the match. Eg `!0` matches an unsuccessful exit.

Comment lines beginning with `#` may be used between tests, but not within them.

Here
[are](http://hub.darcs.net/simon/shelltestrunner/tests)
[some](http://hub.darcs.net/simon/hledger/tests)
<!-- [more](https://github.com/yesodweb/yesod/tree/master/yesod/test) -->
[real](https://github.com/bjpop/berp/tree/master/test/regression)
[world](https://github.com/magthe/cblrepo/tree/master/tests)
[examples](http://code.google.com/p/eddie/source/browse/#hg%2Ftests).

<!--
### New format (1.4+)

Test files contain one or more test groups consisting of:

- optional standard input, following `<` or `<<<`
- one or more tests. A test consists of:

  - a one-line command, beginning with `$` or `$$$`
  - optional standard output (following `>` or `>>>`) and/or standard error output (following `>2` or `>>>2`) specifications
  - an optional exit status specification (following `>=` or `>>>=`)
-->


## Usage

`shelltest` accepts one or more test file or directory arguments.
A directory means all files below it which have the test file suffix (`.test`, by default).

**Command-line options:**
```bash
$ shelltest --help
```
```
shelltest [OPTIONS] [TESTFILES|TESTDIRS]

Common flags:
  -a --all              Show all failure output, even if large
  -c --color            Show colored output if your terminal supports it
  -d --diff             Show failures in diff format
  -p --precise          Show failure output precisely (good for whitespace)
  -x --exclude=STR      Exclude test files whose path contains STR
     --execdir          Run tests from within the test file’s directory
     --extension=EXT    Filename suffix of test files (default: .test)
  -w --with=EXECUTABLE  Replace the first word of (unindented) test commands
     --debug            Show debug info, for troubleshooting
     --debug-parse      Show test file parsing info and stop
     --help-format      Display test format help
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

     -- TFOPTIONS       Set extra test-framework options like -j/--threads,
                        -t/--select-tests, -o/--timeout, --hide-successes.
                        Use -- --help for a list. Avoid spaces.
```

Test commands normally run within your current directory; `--execdir`
makes them run within the directory where they are defined, instead.

`-w/--with` replaces the first word of all test commands with something
else, which can be useful for testing alternate versions of a
program. Test commands which have been indented by one or more spaces will
not be affected by this option.

The test-framework library provides additional options which you can
specify after `--` (note: avoid spaces between flags and values here.)
Run `shelltest -- --help` for a list. Here are some useful ones:
```
  -j NUM  --threads=NUMBER    number of threads to use to run tests
  -o NUM  --timeout=NUMBER    how many seconds a test should be run for before giving up
  -t PAT  --select-tests=PAT  only tests that match at least one glob pattern given by
                               an instance of this argument will be run
          --hide-successes    hide sucessful tests, and only show failures
````

**Example:**

Run

- the tests defined in any `*.test` file in or below the `tests/` directory (`tests`),
- in colour if possible (`-c`),
- whose names<sup><small>2</small></sup> contain "`args`" (`-- -targs`),
- with up to 8 tests running in parallel (`-- -j8`),
- allowing no more than 1 second for each test (`-- -o1`),
- reporting only the failures (`-- --hide-successes`):

````bash
$ shelltest tests -c -- -targs -j8 -o1 --hide
````

<a name="note2"><small><sup>2</sup>
A test's name is what you see when running tests, ie the file name plus the sequence number within the file.
</small></a>

## Contributing

The released version is on [hackage](http://hackage.haskell.org/package/shelltestrunner)
([Release notes](http://hackage.haskell.org/package/shelltestrunner/changelog)).

The latest code is on darcs hub<sup><small>3</small></sup>
([browse](http://hub.darcs.net/simon/shelltestrunner),
[changes](http://hub.darcs.net/simon/shelltestrunner/changes)).
Clone it with:

```bash
$ darcs get http://hub.darcs.net/simon/shelltestrunner
```

<div id="donate-buttons" style="float:right; padding-left:1em;">
<a title="Donate via Gittip" href="https://www.gittip.com/simonmichael"><img src="/site/gittip.png" alt="Gittip"></a>
<a style="margin-left:1em;" title="Donate via Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=2PLCMR67L4G3E"><img src="/site/paypal.gif" alt="Paypal"></a>
</div>
Feedback, testing, code, documentation, packaging, blogging are most welcome.
Here's the
<!-- [2012 user survey](https://docs.google.com/spreadsheet/viewform?formkey=dGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE6MA#gid=3) -->
[2012 user survey](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html).
[Email](mailto:simon@joyful.com?subject=shelltestrunner) or
[chat](irc://irc.freenode.net/#haskell) me (`sm` on irc.freenode.net).

<a name="note3"><small><sup>3</sup>
For help with darcs see [here](http://hub.darcs.net).
</small></a>

## Credits

[Simon Michael](http://joyful.com) wrote shelltestrunner, inspired by John Wiegley's test system for Ledger.

Code contributors include:
John Macfarlane,
Bernie Pope,
Trygve Laugstøl,
Iustin Pop,
Sergei Trofimovich,
Andrés Sicard-Ramírez,
John Chee.

shelltestrunner depends on several fine libraries, in particular Max
Bolingbroke's test-framework, and of course on the Glorious Haskell
Compiler.

The Blade Runner font is by Phil Steinschneider.

<!-- http://www.explore-science-fiction-movies.com/blade-runner-movie-quotes.html -->
