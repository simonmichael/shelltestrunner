# shelltestrunner

This tool aims to help with repeatable testing of a command-line program
(or any shell command), via declarative \"shell tests\" which are easier
to write than procedural shell scripts.  Tests are defined in one or more
test files, which typically have the .test suffix and live in a tests/
subdirectory. Each test specifies a command line, optional standard input,
and expected standard output, error output and/or exit status.  Tests can
be run in parallel for greater speed. shelltestrunner was inspired by the
tests in John Wiegley's ledger project.

Compatibility: should work on microsoft windows as well as unix; not well
tested on windows. Should build with ghc 6.10; for unicode support,
requires ghc 6.12.

# Usage

>     shelltest [FLAG] [TESTFILES|TESTDIRS]
>     
>       -? --help[=FORMAT]           Show usage information (optional format)
>       -V --version                 Show version information
>       -v --verbose                 Higher verbosity
>       -q --quiet                   Lower verbosity
>       -d --debug                   show debug messages
>          --debug-parse             show parsing debug messages and stop
>       -c --color                   display with ANSI color codes
>          --execdir                 run tests in same directory as test file
>          --extension=EXT           extension of test files when dirs specified (default=.test)
>       -i --implicit=none|exit|all  provide implicit tests (default=exit)
>       -w --with=EXECUTABLE         alternate executable, replaces the first word of test commands
>          =OTHER FLAGS              any other flags are passed to test runner
>     
>     A test file contains one or more shell tests, which look like this:
>     
>      # optional comment lines
>      a one-line shell command to be tested
>      <<<
>      stdin lines
>      >>> [/regexp to match in stdout/]
>      [or expected stdout lines
>      >>>2 [/regexp to match in stderr/]
>      [or expected stderr lines]
>      >>>= expected exit status or /regexp/
>     
>     The command line is required; all other fields are optional.
>     The expected stdout (>>>) and expected stderr (>>>2) fields can have either
>     a regular expression match pattern, in which case the test passes if the
>     output is matched, or 0 or more data lines, in which case the output
>     must match these exactly. The expected exit status (>>>=) field can have
>     either a numeric exit code or a /regexp/. A ! preceding a /regexp/ or exit
>     code negates the match. The regular expression syntax is that of the
>     pcre-light library with the dotall flag.
>     
>     By default there is an implicit test for exit status=0, but no implicit test
>     for stdout or stderr.  You can change this with -i/--implicit-tests.
>     
>     The command runs in your current directory unless you use --execdir.
>     You can use --with/-w to replace the first word of command lines
>     (everything up to the first space) with something else, eg to test a
>     different version of your program. To prevent this, start the command line
>     with a space.
>     
>     Any unrecognised options will be passed through to test-framework's runner.
>     You may be able to get a big speedup by running tests in parallel: try -j8.

# Release notes

**0.9 2010/9/3**

  * show plain non-ansi output by default, add --color option

  * better handling of non-ascii test data. We assume that non-ascii file
    paths, command-line arguments etc. are UTF-8 encoded on unix systems
    (cf http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html),
    and that GHC 6.12 or greater is used. Then:

    - non-ascii test file paths should render correctly, eg in failure messages
    - non-ascii test commands should run correctly
    - non-ascii expected output should match correctly
    - non-ascii regular expressions should match correctly. (Caveat: not
      thoroughly tested, this may break certain regexps, )

  * use regex-tdfa instead of pcre-light for better windows compatibility
    To avoid a memory leak in current regex-tdfa, only regular expressions
    up to 300 characters in size are supported. Also, DOTALL is no longer
    enabled and probably fewer regexp constructs are supported.  There are
    still issues on windows/wine but in theory this will help.

  * tighten up dependencies

**0.8 2010/4/9**

  * rename executable to shelltest. The package might also be renamed at some point.

  * better built-in help

  * shell tests now include a full command line, making them more readable
    and self-contained. The --with option can be used to replace the first
    word with something else, unless the test command line begins with a
    space.

  * we also accept directory arguments, searching for test files below
    them, with two new options:
      --execdir        execute tested command in same directory as test file
      --extension=EXT  file extension of test files (default=.test)

**0.7 2010/3/5**

  * more robust parsing
    - --debug-parse parses test files and stops
    - regexps now support escaped forward slash (\/)
    - bad regexps now fail at startup
    - command-line arguments are required in a test, and may be blank
    - a >>>= is no longer required to separate multiple tests in a file
    - comments can be appended to delimiter lines
    - comments can appear at end of file
    - files need not have a final newline
    - files containing nothing, all comments, or valid tests are allowed; anything else is rejected
    - somewhat better errors
    - allow indented input

  * support negative (-) and negatively-matched (!) numeric exit codes

  * let . in regexps match newline

  * warn but continue when a test file fails to parse

  * output cleanups, trim large output

  * more flexible --implicit flag

  * switch to the more robust and faster pcre-light regexp lib

**0.6 2009/7/15**

  * allow multiple tests per file, handle bad executable better

**0.5 2009/7/14**

  * show failure output in proper order

**0.4 2009/7/14**

  * run commands in a more robust way to avoid hangs
    This fixes hanging when a command generates large output, and hopefully
    all other deadlocks. The output is consumed strictly. Thanks to Ganesh
    Sittampalam for his help with this.

  * --implicit-tests flag providing implicit tests for omitted fields

  * --debug flag

  * regular expression matching

  * disallow interspersed foreign options which confused parseargs

  * change comment character to #

**0.3 2009/7/11**

  * misc. bugfixes/improvements

**0.2 2009/7/10**

  * bugfix, build with -threaded

**0.1 2009/7/10**

  * shelltestrunner, a generic shell command stdout/stderr/exit status tester
