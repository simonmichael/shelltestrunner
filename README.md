---
title: shelltestrunner: command-line testing
---

<a href="http://github.com/simonmichael/shelltestrunner">
<img style="position: absolute; top: 0; right: 0; border: 0;" src="http://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png" alt="Fork me on GitHub" />
</a>

<div id=title>
<h1>Command-line testing</h1>
<img src="site/title2.png">
</div>

* toc

*Human or Replicant ??*

If you have a testing situation such as this<sup><small>1</small></sup>,
we at [Joyful Corp.](http://joyful.com) can help!

**shelltestrunner** is a portable command-line tool for testing other
command-line programs, or general shell commands, on (eg) Unix, Mac or
Windows.  It reads simple test specifications defining a command to
run, some input, and the expected output, stderr, and exit status.  It
can run tests in parallel, selectively, with a timeout, in color, etc.
shelltestrunner is free software released under GPLv3+.

<a name="note1">
<small><sup>1</sup> expressed on the command line</small>
</a>


## Quick start

---------------------------------|--------------------------------------
Debian,&nbsp;Ubuntu:&nbsp;&nbsp; | **`apt-get install shelltestrunner`**
Gentoo:                          | **`emerge shelltestrunner`**
Elsewhere:                       | [Get haskell](http://www.stackage.org/install), **`cabal install shelltestrunner`**

Here's a simple test file, `true.test`, containing two tests:

```bash
# true, given no input, prints nothing on stdout or stderr and
# terminates with exit status 0.
$$$ true

# false gives exit status 1.
$$$ false
>>>= 1
```

Here's another test file, `cat.test`, with three tests sharing the same input (`foo`):

```bash
# cat copies its input to stdout
<<<
foo
$$$ cat
>>>
foo

# or, given a bad flag, prints a platform-specific error and exits with non-zero status
$$$ cat --no-such-flag
>>>2 /(unrecognized|illegal) option/
>>>= !0

# echo ignores the input and prints a newline.
# We use an explicit >>>2 (or >>>=) to delimit the whitespace which
# would otherwise be ignored.
$$$ echo
>>>

>>>2
```

The `<<<` and `>>>` delimiters can often be omitted, so the above can also be written as:
```bash
foo
$$$ cat
foo

$$$ cat --no-such-flag
>>>2 /(unrecognized|illegal) option/
>>>= !0

$$$ echo
>>>

>>>2
```

To run the tests, specify the files or a parent folder:

```bash
$ shelltest *.test  # (or shelltest .)
:cat.test:1: [OK]
:cat.test:2: [OK]
:cat.test:3: [OK]
:true.test:1: [OK]
:true.test:2: [OK]

         Test Cases  Total      
 Passed  5           5          
 Failed  0           0          
 Total   5           5          
```

## Usage

`shelltest` accepts one or more test file or directory arguments.
A directory means all files below it which have the test file suffix,
normally `.test`.

**Command-line options:**
```bash
$ shelltest --help
shelltest 1.9.98
```
```
shelltest [OPTIONS] [TESTFILES|TESTDIRS]

Common flags:
  -l --list             List all parsed tests by name
  -a --all              Don't truncate output, even if large
  -c --color            Show colored output if your terminal supports it
  -d --diff             Show expected output mismatches in diff format
  -p --precise          Show expected/actual output precisely (eg whitespace)
  -h --hide-successes   Show only test failures
  -i --include=PAT      Include tests whose name contains this glob pattern
  -x --exclude=STR      Exclude test files whose path contains STR
     --execdir          Run tests from within the test file's directory
     --extension=EXT    File suffix of test files (default: .test)
  -w --with=EXECUTABLE  Replace the first word of (unindented) test commands
  -o --timeout=SECS     Number of seconds a test may run (default: no limit)
  -j --threads=N        Number of threads for running tests (default: 1)
     --debug            Show debug info, for troubleshooting
     --debug-parse      Show test file parsing info and stop
     --help-format      Describe the test file format
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
```

Test commands run from within your current directory by default;
with `--execdir` they run from the directory where they are defined, instead.

`--include` selects only tests whose name (file name plus intra-file sequence number) matches a
[.gitignore-style pattern](https://batterseapower.github.io/test-framework#the-console-test-runner),
while `--exclude` skips tests based on their file path.
These can be used eg to focus on a particular test, or to skip tests intended for a different platform.

`-w/--with` replaces the first word of all test commands with something
else, which can be useful for testing alternate versions of a
program. Commands which have been prefixed by an extra space will
not be affected by this option.

`--hide-successes` gives quieter output, reporting only failed tests.

An example:
````bash
$ shelltest tests -i args -c -j8 -o1 --hide
````
This runs

- the tests defined in any `*.test` file in or below the `tests/` directory
- whose names contain "`args`"
- in colour if possible
- with up to 8 tests running in parallel
- allowing no more than 1 second for each test
- showing only the failures (long flags like `--hide-successes` can be abbreviated)

## Test format

Test files contain one or more test groups. A test group consists of
some optional standard input and one or more tests.  Each test
is a one-line shell command followed by optional expected standard
output, error output and/or numeric exit status, separated by
delimiters.  Use `shelltestrunner --help-format` to see a quick
reference:

```bash
# COMMENTS OR BLANK LINES
<<<
INPUT
$$$ COMMAND LINE
>>>
EXPECTED OUTPUT (OR >>> /REGEX/)
>>>2
EXPECTED STDERR (OR >>>2 /REGEX/)
>>>= EXPECTED EXIT STATUS (OR >>>= /REGEX/ OR >>>=)
# COMMENTS OR BLANK LINES
ADDITIONAL TESTS FOR THIS INPUT
ADDITIONAL TEST GROUPS WITH DIFFERENT INPUT
```

All parts are optional except the command line.
If not specified, stdout and stderr are expected to be empty
and exit status is expected to be zero.

There are some additional conveniences:

The `<<<` delimiter is optional for the first input in a file.
Without it, input begins at the first non-blank/comment line.
Input ends at the `$$$` delimiter. You can't put a comment before the first `$$$`.

The `>>>` delimiter is optional except when matching via regex.
Expected output/stderr extends to the next `>>>2` or `>>>=` if present,
or to the last non-blank/comment line before the next `<<<` or `$$$` or file end.
`>>>=` with nothing after it ignores the exit status.

Two spaces between `$$$` and the command protects it from -w/--with (see below).

`/REGEX/` regular expression patterns may be used instead of
specifying the expected output in full. The regex syntax is
[regex-tdfa](http://hackage.haskell.org/package/regex-tdfa)'s, plus
you can put `!` before `/REGEX/` to negate the match.

The [exit status](http://en.wikipedia.org/wiki/Exit_status) is a
number, normally 0 for a successful exit.  This too can be prefixed
with `!` to negate the match, or you can use a `/REGEX/`.

<h3>Format 2b (short delimiters)</h3>

If the above fails to parse a file, shelltestrunner will try test format 2b,
which is just the same but with short delimiters: `<` `$` `>` `>2` `>=`.

<h3>Format 1 (deprecated)</h3>

Finally, it will try 1.x's format for backward compatibility.
Here, test files contain one or more individual tests, each consisting
of a one-line shell command, optional input, expected standard output
and/or error output, and a (required) exit status.

```bash
# COMMENTS OR BLANK LINES
COMMAND LINE
<<<
INPUT
>>>
EXPECTED OUTPUT (OR >>> /REGEXP/)
>>>2
EXPECTED STDERR (OR >>>2 /REGEXP/)
>>>= EXPECTED EXIT STATUS (OR >>>= /REGEXP/)
```

When not specified, stdout/stderr are ignored.
A space before the command protects it from -w/--with.

[Here](https://github.com/simonmichael/shelltestrunner/tree/master/tests.format1)
[are](https://github.com/simonmichael/shelltestrunner/tree/master/tests.format2)
[some](https://github.com/simonmichael/shelltestrunner/tree/master/tests.format2b)
[real](https://github.com/simonmichael/hledger/tree/master/tests)
[world](https://github.com/bjpop/berp/tree/master/test/regression)
[examples](https://github.com/magthe/cblrepo/tree/master/tests).


## Contribute

<div id="donate-buttons" style="float:right; padding-left:1em;">
<a title="Donate via Gittip" href="https://www.gittip.com/simonmichael"><img src="/site/gittip.png" alt="Gittip"></a>
<a style="margin-left:1em;" title="Donate via Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=2PLCMR67L4G3E"><img src="/site/paypal.gif" alt="Paypal"></a>
</div>
The released version is on [hackage](http://hackage.haskell.org/package/shelltestrunner)
([changelog](http://hackage.haskell.org/package/shelltestrunner/changelog)).
The [code](https://github.com/simonmichael/shelltestrunner)
and [issues](https://github.com/simonmichael/shelltestrunner/issues)
are on github.
Feedback, testing, code, documentation, packaging, blogging are most welcome.
Here's the
<!-- [2012 user survey](https://docs.google.com/spreadsheet/viewform?formkey=dGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE6MA#gid=3) -->
[2012 user survey](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html).
[Email](mailto:simon@joyful.com?subject=shelltestrunner) or
[chat](irc://irc.freenode.net/#haskell) me (`sm` on irc.freenode.net).

## Credits

[Simon Michael](http://joyful.com) wrote shelltestrunner,
which was originally hledger's test tool, which was
inspired by John Wiegley's test system for Ledger.

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
