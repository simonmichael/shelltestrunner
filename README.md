<div align=center>
<h1 style="margin:0;">Easy, repeatable testing of CLI programs/commands</h1>
<img src="site/title2.png">

  [Install](#install)
| [Usage](#usage)
| [Options](#options)
| [Test formats](#test-formats)
| [Support/Contribute](#supportcontribute)
| [Credits](#credits)
</div>

**shelltestrunner** (executable: `shelltest`) is a portable
command-line tool for testing command-line programs, or general shell
commands, released under GPLv3+.  It reads simple test specifications
defining a command to run, some input, and the expected output,
stderr, and exit status.  It can run tests in parallel, selectively,
with a timeout, in color, include or exclude particular tests, etc. 
Projects using it include
[hledger](http://hledger.org),
[Agda](http://wiki.portal.chalmers.se/agda),
and
[berp](https://github.com/bjpop/berp).

## Install

There may be a new-enough 
[packaged version](https://repology.org/metapackage/shelltestrunner/badges)
on your platform. Eg:

|||
|----------------|---------------------------------------
| Debian/Ubuntu: | **`apt install shelltestrunner`**
| Gentoo:        | **`emerge shelltestrunner`**

Or, build the latest release on any major platform:

|||
|----------------|---------------------------------------
| stack:         | **[get stack](https://haskell-lang.org/get-started)**, **`stack install shelltestrunner-1.9`**
| cabal:         | **`cabal update; cabal install shelltestrunner-1.9`**

## Usage

Here's a minimal test file containing one shell test:

    $ cat echo.test
    # A comment. Testing bash's builtin "echo" command (if /bin/sh is bash)
    echo
    >>>= 0

They're called "shell test" because any shell (`/bin/sh` on POSIX, `CMD` on Windows)
command line can be tested.
Each test begins with the command to test, followed by optional stdin input, 
expected stdout and/or stderr output, and ends with the expected exit status.
Here's another file containing two tests:

    $ cat cat.test
    # Test that the "cat" program copies its input to stdout, 
    # nothing appears on stderr, and exit status is 0.
    cat
    <<<
    foo
    >>>
    foo
    >>>2
    >>>= 0
    
    # Test that cat prints an error containing "unrecognized option" 
    # (and exits with non-zero status) if given a bad flag.
    cat --no-such-flag
    >>>2 /unrecognized option/
    >>>= !0

To run these tests:

    $ shelltest echo.test cat.test
    :echo.test: [OK]
    :cat.test:1: [OK]
    :cat.test:2: [OK]

             Test Cases  Total      
     Passed  3           3          
     Failed  0           0          
     Total   3           3          

That's the basics! 
There are also some alternate test formats you'll read about below.

## Options

    $ shelltest --help
    shelltest 1.9

    shelltest [OPTIONS] [TESTFILES|TESTDIRS]

    Common flags:
      -l --list             List all parsed tests by name
      -a --all              Don't truncate output, even if large
      -c --color            Show colored output if your terminal supports it
      -d --diff             Show expected output mismatches in diff format
      -p --precise          Show expected/actual output precisely (eg whitespace)
      -h --hide-successes   Show only test failures
         --xmlout=FILE      Specify file to store test results in xml format.
      -D --defmacro=D=DEF   Specify a macro that is evaluated by preprocessor
                            before the test files are parsed. D stands for macro
                            definition that is replaced with the value of DEF.
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

`shelltest` accepts one or more test file or directory arguments.
A directory means all files below it named `*.test` (customisable with `--extension`).

Test commands are run with `/bin/sh` on POSIX systems and with `CMD` on Windows.
By default, they are run in the directory in which you ran `shelltest`;
with `--execdir` they will run in each test file's directory instead.

`--include` selects only tests whose name (file name plus intra-file sequence number) matches a
[.gitignore-style pattern](https://batterseapower.github.io/test-framework#the-console-test-runner),
while `--exclude` skips tests based on their file path.
These can be used eg to focus on a particular test, or to skip tests intended for a different platform.

`-D/--defmacro` defines a macro that is replaced by preprocessor before any tests are parsed and run.

`-w/--with` replaces the first word of all test commands with something
else, which can be useful for testing alternate versions of a
program. Commands which have been prefixed by an extra space will
not be affected by this option.

`--hide-successes` gives quieter output, reporting only failed tests.

Long flags can be abbreviated to a unique prefix.
 

For example, the command:

    $ shelltest tests -i args -c -j8 -o1 -DCONF_FILE=test/myconf.cfq --hide

- runs the tests defined in any `*.test` file in or below the `tests/` directory
- whose names contain "`args`"
- in colour if possible
- with up to 8 tests running in parallel
- allowing no more than 1 second for each test
- replacing the text "`CONF_FILE`" in all tests with "`test/myconf.cfq`"
- reporting only the failures.

## Test formats

At present several test file formats are supported, and experimentation continues.
The current formats will remain supported or have a migration path.
`shelltestrunner --help-format` shows a quick reference.
shelltestrunner tries to parse test files using the formats below, in this order: 
format 2, format 3, format 1.

### Format 1

This is the original format used by shelltestrunner up to version
1.3.x.  Test files contain one or more individual tests, each
consisting of a one-line shell command, optional input, expected
standard output and/or error output, and a (required) exit status.

Syntax:

    # COMMENTS OR BLANK LINES
    COMMAND LINE
    <<<
    INPUT
    >>>
    EXPECTED OUTPUT (OR >>> /REGEXP/)
    >>>2
    EXPECTED STDERR (OR >>>2 /REGEXP/)
    >>>= EXPECTED EXIT STATUS (OR >>>= /REGEXP/)

When not specified, stdout/stderr are ignored.
A space before the command protects it from -w/--with.

Examples: 
[shelltestrunner](https://github.com/simonmichael/shelltestrunner/tree/master/tests/format1),
[hledger](https://github.com/simonmichael/hledger/tree/master/tests),
[berp](https://github.com/bjpop/berp/tree/master/test/regression),
[cblrepo](https://github.com/magthe/cblrepo/tree/master/tests).

### Format 2

Format 2 allows multiple tests to share the same input (shelltestrunner 1.9+).

Syntax:

Test files contain one or more test groups. A test group consists of
some optional standard input and one or more tests.  Each test
is a one-line shell command followed by optional expected standard
output, error output and/or numeric exit status, separated by
delimiters.

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

All test parts are optional except the command line.
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

Examples:

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

The `<<<` and `>>>` delimiters can often be omitted, so the above can also be written as:

    foo
    $$$ cat
    foo

    $$$ cat --no-such-flag
    >>>2 /(unrecognized|illegal) option/
    >>>= !0

    $$$ echo
    >>>

    >>>2

### Format 3

Format 3 is like format 2, but with shorter delimiters: < > >2 >= (shelltestrunner 1.9+).

Syntax:

    # COMMENTS OR BLANK LINES
    <
    INPUT
    $ COMMAND LINE
    >
    EXPECTED OUTPUT (OR > /REGEX/)
    >2
    EXPECTED STDERR (OR >2 /REGEX/)
    >= EXPECTED EXIT STATUS (OR >= /REGEX/ OR >=)
    # COMMENTS OR BLANK LINES
    ADDITIONAL TESTS FOR THIS INPUT
    ADDITIONAL TEST GROUPS WITH DIFFERENT INPUT

Examples: 
[shelltestrunner](https://github.com/simonmichael/shelltestrunner/tree/master/tests/format3)


## Support/Contribute

|||
|----------------------|--------------------------------------------------|
| Released version:    | http://hackage.haskell.org/package/shelltestrunner
| Changelog:           | http://hackage.haskell.org/package/shelltestrunner/changelog
| Code                 | https://github.com/simonmichael/shelltestrunner
| Issues               | https://github.com/simonmichael/shelltestrunner/issues
| IRC                  | http://webchat.freenode.net?channels=shelltestrunner
<!-- | Email                | [simon@joyful.com](mailto:simon@joyful.com?subject=[shelltestrunner]) -->

[2012 user survey](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html).

Feedback, testing, code, documentation, packaging, blogging, and funding are most welcome.

<div id="donate-buttons">
<a title="Donate via Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=2PLCMR67L4G3E"><img src="/site/paypal.gif" alt="Paypal"></a>
</div>

## Credits

[Simon Michael](http://joyful.com) wrote shelltestrunner,
inspired by John Wiegley's tests for Ledger.

Code contributors include:
John Macfarlane,
Bernie Pope,
Trygve Laugstøl,
Iustin Pop,
Sergei Trofimovich,
Andrés Sicard-Ramírez,
Taavi Väljaots,
John Chee.

shelltestrunner depends on several fine libraries, in particular Max
Bolingbroke's test-framework, and of course on the Glorious Haskell
Compiler.

The Blade Runner font is by Phil Steinschneider.

<!-- http://www.explore-science-fiction-movies.com/blade-runner-movie-quotes.html -->
