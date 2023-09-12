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
with a timeout, in color, etc. 

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
| stack:         | **[get stack](https://docs.haskellstack.org)**, **`stack install shelltestrunner-1.10`**
| cabal:         | **`cabal update; cabal install shelltestrunner-1.10`**

## Usage

Here's a test file containing three simple tests.
They're called "shell tests" because any shell command line can be tested.
A test contains:

- `<` and one or more lines of input to be provided on stdin (optional)
- `$` and the command line to test (required)
- zero or more lines of expected stdout output, or a regexp (optional)
- `>2` and zero or more lines of expected stderr output, or a regexp (optional)
- `>=` and an expected exit code, or a regexp (optional)

<!-- keep synced with tests/examples -->
```
# 1. Test that the "echo" command (a shell builtin, usually)
# prints its argument on stdout, prints nothing on stderr,
# and exits with a zero exit code.

$ echo a
a

# 2. Test that echo with no arguments prints a blank line,
# no stderr output, and exits with zero.
# Since the output ends with whitespace, this time we must write
# the exit code test (>=) explicitly, to act as a delimiter.

$ echo

>=

# 3. Test that cat with a bad flag prints nothing on stdout,
# an error containing "unrecognized option" or "illegal option" on stderr,
# and exits with non-zero status.

$ cat --no-such-flag
>2 /(unrecognized|illegal) option/
>= !0

```

To run these tests:

```
$ shelltest examples.test
:examples.test:1: [OK]
:examples.test:2: [OK]
:examples.test:3: [OK]

         Test Cases  Total 
 Passed  3           3     
 Failed  0           0     
 Total   3           3     
```

That's the basics! 
There are also some alternate test formats you'll read about below.

## Options

<!-- shelltest --help | sed -e '/^shelltest file formats/,$d' -->
```
shelltest 1.10

shelltest [OPTIONS] [TESTFILES|TESTDIRS]

Common flags:
  -l --list             List the names of all tests found
  -i --include=PAT      Include tests whose name contains this glob pattern
                        (eg: -i1 -i{4,5,6})
  -x --exclude=STR      Exclude test files whose path contains STR
  -a --all              Show all output without truncating, even if large
  -c --color            Show colored output if your terminal supports it
  -d --diff             Show differences between expected/actual output
  -p --precise          Show expected/actual output precisely, with quoting
     --hide-successes   Show only test failures
  -f --fail-fast        Only hspec: stop tests on first failure
     --xmlout=FILE      Save test results to FILE in XML format.
  -D --defmacro=D=DEF   Define a macro D to be replaced by DEF while parsing
                        test files.
     --execdir          Run tests from within each test file's directory
     --extension=EXT    File suffix of test files (default: .test)
  -w --with=EXE         Replace the first word of test commands with EXE
                        (unindented commands only)
  -o --timeout=SECS     Number of seconds a test may run (default: no limit)
  -j --threads=N        Number of threads for running tests (default: 1)
     --shell=EXE        The shell program to use (must accept -c CMD;
                        default: /bin/sh on POSIX, cmd.exe on Windows)
     --debug            Show debug info while running
     --debug-parse      Show test file parsing results and stop
Print test file:
     --print[=FORMAT]   Print test files in specified format (default: v3).
     --hspec            Use hspec to run tests.
  -h --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number

```
    
`shelltest` accepts one or more test file or directory arguments.
A directory means all files below it named `*.test` (customisable with `--extension`).

By default, test commands are run with `/bin/sh` on POSIX systems
and with `CMD` on Windows; you can change this with the `--shell` option.

By default, tests run in the directory in which you ran `shelltest`;
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

shelltestrunner supports three test file formats:

| Format name           | Description                                                   | Delimiters, in order       |
|-----------------------|---------------------------------------------------------------|----------------------------|
| format 1 (deprecated) | command is first; exit status is required                     | `(none) <<< >>> >>>2 >>>=` |
| format 2 (verbose)    | input is first, can be reused; all but command can be omitted | `<<<    $$$ >>> >>>2 >>>=` |
| format 3 (preferred)  | same as format 2 but with short delimiters                    | `<      $   >   >2   >=`   |

To read each file, shelltestrunner tries the formats in this order: format 2, then format 3, then format 1.
Within a file, all tests should use the same format.

Here are the formats in detail, from oldest to newest.
You should use format 3; or if that clashes with your data, then format 2.

### Format 1

This old format is included for backward compatibility with old tests.

Test files contain one or more individual tests, each consisting of a
one-line shell command, optional input, expected standard output
and/or error output, and a (required) exit status.

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
[Agda](https://github.com/agda/agda/tree/master/src/size-solver/test),
[berp](https://github.com/bjpop/berp/tree/master/test/regression),
[cblrepo](https://github.com/magthe/cblrepo/tree/master/tests).

### Format 2

This is supported by shelltestrunner 1.9+. 
It improves on format 1 in two ways: it allows tests to reuse the same input,
and it allows delimiters/test clauses to be omitted, with more useful defaults.

Test files contain one or more test groups. 
A test group consists of some optional standard input and one or more tests.
Each test is a one-line shell command followed by optional expected standard output, 
error output and/or numeric exit status, separated by delimiters.

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

Two spaces between `$$$` and the command protects it from -w/--with.

The `<<<` delimiter is optional for the first input in a file.
Without it, input begins at the first non-blank/comment line.
Input ends at the `$$$` delimiter. You can't put a comment before the first `$$$`.

The `>>>` delimiter is optional except when matching via regex.
Expected output/stderr extends to the next `>>>2` or `>>>=` if present,
or to the last non-blank/comment line before the next `<<<` or `$$$` or file end.
`/REGEX/` regular expression patterns may be used instead of
specifying the expected output in full. The regex syntax is
[regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)'s, plus
you can put `!` before `/REGEX/` to negate the match.

The [exit status](https://en.wikipedia.org/wiki/Exit_status) is a
number, normally 0 for a successful exit.  This too can be prefixed
with `!` to negate the match, or you can use a `/REGEX/` pattern.
A `>>>=` with nothing after it ignores the exit status.

Examples: <!-- keep synced with tests/examples -->

All delimiters explicit:

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
    # We need the >>>= (or a >>>2) to delimit the whitespace which
    # would otherwise be ignored.
    $$$ echo
    >>>

    >>>=

Non-required `<<<` and `>>>` delimiters omitted:

    foo
    $$$ cat
    foo

    $$$ cat --no-such-flag
    >>>2 /(unrecognized|illegal) option/
    >>>= !0

    $$$ echo

    >>>=

### Format 3

This is supported by shelltestrunner 1.9+. 
It is the preferred format - like format 2 but with more convenient short delimiters:

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

Examples: <!-- keep synced with tests/examples -->

All delimiters explicit:

    # cat copies its input to stdout
    <
    foo
    $ cat
    >
    foo

    # or, given a bad flag, prints a platform-specific error and exits with non-zero status
    $ cat --no-such-flag
    >2 /(unrecognized|illegal) option/
    >= !0

    # echo ignores the input and prints a newline.
    # We use an explicit >= (or >2) to delimit the whitespace which
    # would otherwise be ignored.
    $ echo
    >

    >=

Non-required `<` and `>` delimiters omitted:

    foo
    $ cat
    foo

    $ cat --no-such-flag
    >2 /(unrecognized|illegal) option/
    >= !0

    $ echo

    >2

Also:
[above](#usage),
[shelltestrunner](https://github.com/simonmichael/shelltestrunner/tree/master/tests/format3),
[hledger](https://github.com/simonmichael/hledger/tree/master/hledger/test).

## Printing tests

The `--print` option prints tests to stdout.
This can be used to convert between test formats.
Format 1, 2, and 3 are supported.

Here are some issues to be aware of when converting between formats:

- Printing v1 as v2/v3
  - A `>>>= 0` often gets converted to a `>>>2 //` or `>2 //`, when `>=` or nothing would be preferred.
    This is semantically accurate, because v1 ignores out/err by default, and v2/v3 check for zero exit by default,
    and therefore the safest conversion; but it's annoying
- Printing v3 as v3
  - loses comments at the top of the file, even above an explicit < delimiter
  - may lose other data
- A missing newline at EOF will not be preserved.
- v2/v3 allow shared input, but v1 does not
- A file containing only comments may be emptied

In general, always review the result of a conversion yourself before committing it.

## Support/Contribute

|||
|----------------------|--------------------------------------------------|
| Released version:    | https://hackage.haskell.org/package/shelltestrunner
| Changelog:           | https://hackage.haskell.org/package/shelltestrunner/changelog
| Code                 | https://github.com/simonmichael/shelltestrunner
| Issues               | https://github.com/simonmichael/shelltestrunner/issues
| Chat                 | Contact sm in the #hledger:matrix.org room on matrix or the #hledger channel on libera.chat
<!-- | Email                | [simon@joyful.com](mailto:simon@joyful.com?subject=[shelltestrunner]) -->

[2012 user survey](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html).

Feedback, testing, code, documentation, packaging, blogging, and funding are most welcome.

<div id="donate-buttons">
<a title="Donate via Paypal" href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=2PLCMR67L4G3E"><img src="/site/paypal.gif" alt="Paypal"></a>
</div>

## Credits

[Simon Michael](https://joyful.com) wrote shelltestrunner,
inspired by John Wiegley's tests for Ledger.

Code contributors:
Andreas Abel,
Andrés Sicard-Ramírez,
Bernie Pope,
Felix C. Stegerman,
Iustin Pop,
Jakob Schöttl
John Chee.
John Macfarlane,
Sergei Trofimovich,
Taavi Väljaots,
Trygve Laugstøl,

shelltestrunner depends on several fine libraries,
in particular Max Bolingbroke's test-framework,
and of course on the Glorious Haskell Compiler.

The Blade Runner font is by Phil Steinschneider.

<!-- https://www.explore-science-fiction-movies.com/blade-runner-movie-quotes.html -->
