---
title: shelltestrunner
---

# shelltestrunner

shelltestrunner tests command-line programs (or arbitrary shell commands.)
It reads simple declarative tests specifying a command, some input, and
the expected output, and can run them run in parallel, selectively, with a
timeout, in color, and/or with differences highlighted. shelltestrunner
has been tested on gnu/linux, mac and windows; projects using it include
hledger, berp, cblrepo and eddie. shelltestrunner is free software
released under GPLv3+.

## Installing

If your machine's packaging system does not provide an
[up-to-date](#release-notes) shelltestrunner, install it with
[cabal](http://hackage.haskell.org/trac/hackage/wiki/CabalInstall) (and if
your packaging system does not provide an up-to-date cabal, get the
[haskell platform](http://hackage.haskell.org/platform/)):

    $ cabal install shelltestrunner

You should now have the `shelltest` program in your path.

## Defining tests

Test files, typically named `tests/*.test`, contain one or more tests
consisting of:

- a one-line command
- optional standard input (`<<<`), standard output (`>>>`) and/or standard error output (`>>>2`) specifications
- an exit status (`>>>=`) specification


**Example:** here's `example.test`, a file containing two simple tests:

    # 1. let's test that echo runs. Numbering your tests can be helpful.
    echo
    >>>= 0

    # 2. and now the cat command. On windows, this one should fail.
    cat
    <<<
    foo
    >>>
    foo
    >>>= 0

Run it with `shelltest`:

    $ shelltest example.test
    :t.test:1: [OK]
    :t.test:2: [OK]
    
             Test Cases  Total
     Passed  2           2
     Failed  0           0
     Total   2           2

**Test format:**

    # optional comment
    the command to test
    <<<
    zero or more lines of standard input
    >>>
    zero or more lines of expected standard output (or /REGEXP/ added to the previous line)
    >>>2
    zero or more lines of expected standard error output (or /REGEXP/ added to the previous line)
    >>>= EXITCODE (or /REGEXP/)

- A `/REGEXP/` pattern may be used instead of explicit data. In this case
  a match anywhere in the output allows the test to pass. The regular
  expression syntax is [regex-tdfa](http://hackage.haskell.org/package/regex-tdfa)'s.
- `EXITCODE` is a numeric
  [exit status](http://en.wikipedia.org/wiki/Exit_status), eg `0` for a
  successful exit.
- You can put `!` before a `/REGEXP/` or `EXITCODE` to negate the match.
- Comment lines beginning with `#` may be used between tests.

Here
[are](http://joyful.com/repos/shelltestrunner/tests)
[some](http://joyful.com/repos/hledger/tests)
<!-- [more](https://github.com/yesodweb/yesod/tree/master/yesod/test) -->
[real-world](https://github.com/bjpop/berp/tree/master/test/regression)
[project](https://github.com/magthe/cblrepo/tree/master/tests)
[tests](http://code.google.com/p/eddie/source/browse/#hg%2Ftests)
to give you ideas.

## Running tests

The `shelltest` program accepts one or more test file or directory paths.
A directory means all files below it with the test file suffix (default: `.test`).

**Example:** run all tests defined in or below the `tests` directory with
`args` in their name, up to 8 in parallel, allowing a maximum of 1 second
each, showing only failures and the final summary:

    $ shelltest tests -- -targs -j8 -o1 --hide
    
             Test Cases   Total
     Passed  2            2
     Failed  0            0
     Total   2            2

**Command-line options:**

    $ shelltest --help
    shelltest 1.2.1

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

         -- TFOPTIONS       Set extra test-framework options like -j/--threads,
                            -t/--select-tests, -o/--timeout, --hide-successes.
                            Use -- --help for a list. Avoid spaces.

- Test commands normally run within your current directory; `--execdir`
  makes them run within the directory where they are defined, instead.

- `-w/--with` replaces the first word of all test commands with something
  else, which can be useful for testing alternate versions of a
  program. Commands which have been indented by one or more spaces will
  not be affected by this option.

- The test-framework library provides additional options which you can
  specify after `--` (note: avoid spaces between flags and values here.)
  Run `shelltest -- --help` for a list. Here are some useful ones:

        -j NUMBER        --threads=NUMBER             number of threads to use to run tests
        -o NUMBER        --timeout=NUMBER             how many seconds a test should be run for before giving up, by default
        -t TEST-PATTERN  --select-tests=TEST-PATTERN  only tests that match at least one glob pattern given by an instance of this argument will be run
                         --hide-successes             hide sucessful tests, and only show failures

## Contributing

 The released version is on [hackage](http://hackage.haskell.org/package/shelltestrunner).
 The latest code
 ([browse](http://joyful.com/darcsden/simon/shelltestrunner/browse/shelltest.hs),
 [changes](http://joyful.com/darcsden/simon/shelltestrunner/changes))
 is here:

    $ darcs get http://joyful.com/repos/shelltestrunner

 Feedback, code, testing, documentation/blogging are most welcome.  
 Add your experience to the [user survey](https://docs.google.com/spreadsheet/viewform?formkey=dGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE6MA#gid=3)
 ([results](https://docs.google.com/spreadsheet/pub?key=0Au47MrJax8HpdGpZSzdhWHlCUkJpR2hjX1MwMWFoUEE&single=true&gid=3&output=html)).  
 [Email](mailto:simon@joyful.com?subject=shelltestrunner) or
 [chat](irc://irc.freenode.net/#haskell) me (`sm` on irc.freenode.net).  
 For focussed support/faster development/good karma you can [hire me](http://joyful.com/) or 
 <a href="https://www.wepay.com/donate/39988?ref=widget&utm_medium=widget&utm_campaign=donation"
    target="_blank" style=margin:0 1em;"
    ><img style="vertical-align:middle;" src="https://www.wepay.com/img/widgets/donate_with_wepay.png" alt="Donate with WePay" /></a>

## Release notes

**1.2.1** (2012/3/12)

  * use the more up-to-date filemanip package for easier Debian packaging

**1.2** (2012/2/26)

  * support latest cmdargs, test-framework, and GHC 7.4
  * more readable non-quoted failure output by default; for quoted output, use `-p/--precise`
  * the `--all`, `--diff` and `--precise` options now interact well

**1.1** (2011/8/25)

  * bump process dependency to allow building with GHC 7.2.1
  * new `-a/--all` flag shows all failure output without truncating

**1.0** (2011/7/23)

  * New home page/docs
  * The `>>>=` field is now required; you may need to add it to your existing tests
  * Input and expected output can now contain lines beginning with `#`
  * Multiple tests in a file may now have whitespace between them
  * The `-i/--implicit` option has been dropped
  * New `-d/--diff` option shows test failures as a unified diff when possible, including line numbers to help locate the problem
  * New `-x/--exclude` option skips certain test files (eg platform-specific ones)
  * Passing arguments through to test-framework is now more robust
  * Fixed: parsing could fail when input contained left angle brackets
  * Fixed: some test files generated an extra blank test at the end

**0.9** (2010/9/3)

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

**0.8** (2010/4/9)

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

**0.7** (2010/3/5)

  * more robust parsing
    - --debug-parse parses test files and stops
    - regexps now support escaped forward slash (`\/`)
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

**0.6** (2009/7/15)

  * allow multiple tests per file, handle bad executable better

**0.5** (2009/7/14)

  * show failure output in proper order

**0.4** (2009/7/14)

  * run commands in a more robust way to avoid hangs
    This fixes hanging when a command generates large output, and hopefully
    all other deadlocks. The output is consumed strictly. Thanks to Ganesh
    Sittampalam for his help with this.
  * --implicit-tests flag providing implicit tests for omitted fields
  * --debug flag
  * regular expression matching
  * disallow interspersed foreign options which confused parseargs
  * change comment character to #

**0.3** (2009/7/11)

  * misc. bugfixes/improvements

**0.2** (2009/7/10)

  * bugfix, build with -threaded

**0.1** (2009/7/10)

  * shelltestrunner, a generic shell command stdout/stderr/exit status tester

## Credits

[Simon Michael](http://joyful.com) wrote and maintains
shelltestrunner. John Macfarlane, Bernie Pope and Trygve Laugstøl have
contributed code. It relies heavily on Max Bolingbroke's test-framework,
[other libraries](http://hackage.haskell.org/package/shelltestrunner), and
of course the Glorious Haskell Compiler. It was inspired by the tests for
John Wiegley's ledger.
