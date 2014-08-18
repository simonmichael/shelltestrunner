---
title: shelltestrunner
---

<div id=title>
<h2>Command-line testing</h2>
<img src="site/title2.png">
</div>

* toc

***Human or Replicant ?***&nbsp;
If you have a testing situation like this <sup><small>1</small></sup>,
we can help.

shelltestrunner is a command-line tool for testing other command-line
programs, or general shell commands, on GNU/Linux, Mac and Windows.

It reads simple test specifications consisting of a command to run,
some input, and the expected stdout, stderr, and exit status output.  It
can run tests in parallel, selectively, with a timeout, with color
output, or with expected/actual differences highlighted.
shelltestrunner is free software released under GPLv3+.

<a name="note1">
<small><sup>1</sup> expressed on the command line</small>
</a>


## Getting started

---------------------------------|------------------------------------
Debian,&nbsp;Ubuntu:&nbsp;&nbsp; | `apt-get install shelltestrunner`
Gentoo:                          | `emerge shelltestrunner`
Elsewhere:                       | Get [GHC](http://haskell.org/ghc) and cabal (or the [Haskell Platform](http://haskell.org/platform)), ensure `~/.cabal/bin` is in your $PATH, and:<br>`cabal install shelltestrunner`.

You should now have the `shelltest` program in your path.

Tests are kept in files with the `.test` suffix by default. Here's a simple test file:
```
# echo, given no input, prints nothing and terminates normally
echo
>>>= 0
```

Here's another, containing two tests:
```
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

Run them with `shelltest`:
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

## Test format

<!-- Two formats are supported: -->

<!-- ### Old format -->

Test files contain one or more tests, which look like this:
```
# optional comment
the command to test, on one line
<<<
zero or more lines of standard input
>>>
zero or more lines of expected standard output (or /REGEXP/ added to the previous line)
>>>2
zero or more lines of expected standard error output (or /REGEXP/ added to the previous line)
>>>= EXITCODE (or /REGEXP/)
```

The command and the exit status are required; the standard input, standard output, and/or standard error output are optional.

A `/REGEXP/` pattern may be used instead of specifying the output
exactly, in which case a match anywhere in the output allows the test
to pass. The regular expression syntax is
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa)'s, plus
you can put `!` before `/REGEXP/` to negate the match.

`EXITCODE` is a numeric [exit status](http://en.wikipedia.org/wiki/Exit_status)
or a `/REGEXP/`. Again, prefix a `!` to negate the match. Eg `!0` matches an unsuccessful exit. 

Comment lines beginning with `#` may be used between tests (but not within them).

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

The `shelltest` program accepts one or more test file or directory arguments.
A directory means all files below it with the test file suffix (default: `.test`).

**Command-line options:**
```bash
$ shelltest --help
shelltest 1.3.4

shelltest [OPTIONS] [TESTFILES|TESTDIRS]

Common flags:
  -a --all              Show all failure output, even if large
  -c --color            Show colored output if your terminal supports it
  -d --diff             Show failures in diff format
  -p --precise          Show failure output precisely (good for whitespace)
  -x --exclude=STR      Exclude test files whose path contains STR
     --execdir          Run tests from within the test file's directory
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
```bash
    -j NUMBER        --threads=NUMBER             number of threads to use to run tests
    -o NUMBER        --timeout=NUMBER             how many seconds a test should be run for before giving up, by default
    -t TEST-PATTERN  --select-tests=TEST-PATTERN  only tests that match at least one glob pattern given by an instance of this argument will be run
                     --hide-successes             hide sucessful tests, and only show failures
````

**Example:**

Run

- the tests defined in any `*.test` file in or below the `tests/` directory (`tests`),
- whose names<sup><small>2</small></sup> contain "`args`" (`-- -targs`),
- with up to 8 tests running in parallel (`-- -j8`),
- allowing a maximum of 1 second for each test (`-- -o1`),
- reporting only the failures (`-- --hide-successes`):

````bash
$ shelltest tests -- -targs -j8 -o1 --hide
````
<a name="note2"><small><sup>2</sup>
A test's name is the name of the file where it was defined plus its sequence number within the file.
</small></a>

## Contributing

 The released version is on [hackage](http://hackage.haskell.org/package/shelltestrunner)
 ([Release notes](http://hackage.haskell.org/package/shelltestrunner/changelog)).

 The latest code is on darcs hub
 ([browse](http://hub.darcs.net/simon/shelltestrunner/shelltest.hs),
 [changes](http://hub.darcs.net/simon/shelltestrunner/changes)).
 Clone it with:

    $ darcs get http://hub.darcs.net/simon/shelltestrunner

 Feedback, code, testing, documentation/blogging are most welcome.
 Here's the
 <!-- [2012 user survey](https://docs.google.com/spreadsheet/viewform?formkey=dGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE6MA#gid=3) -->
 [2012 user survey](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html).  
 [Email](mailto:simon@joyful.com?subject=shelltestrunner) or
 [chat](irc://irc.freenode.net/#haskell) me (`sm` on irc.freenode.net).  

 <a href="https://www.wepay.com/donate/39988?ref=widget&utm_medium=widget&utm_campaign=donation"
    target="_blank" style=margin:0 1em;"
    ><img style="vertical-align:middle;" src="https://www.wepay.com/img/widgets/donate_with_wepay.png" alt="Donate with WePay" /></a>

## Credits

[Simon Michael](http://joyful.com) wrote shelltestrunner, inspired by John Wiegley's test system for Ledger.
Code contributors include
John Macfarlane,
Bernie Pope,
Trygve Laugstøl
and John Chee.
shelltestrunner depends on
several fine [libraries](http://hackage.haskell.org/package/shelltestrunner),
in particular Max Bolingbroke's test-framework,
and of course on the Glorious Haskell Compiler.

<!-- http://www.explore-science-fiction-movies.com/blade-runner-movie-quotes.html -->
