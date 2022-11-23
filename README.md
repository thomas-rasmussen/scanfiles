# scanfiles

Small utility package to facilitate scanning files for patterns.

# TODO
1) Basic functionality in place. Everything needs to be refactored.
2) Need to add tests. Ideally it would be possible to programmatically create directories and files
   in some temporary folder during running of tests, so that all aspects of the functions can be tested
   in an automatic way. Read testthat documentation and chapters 13-15 in https://r-pkgs.org/testing-basics.html.
   At a quick glance, it seems like there is a proper way to set this up.
For now, tests are run manually during development, and skipped using 
testthat::skip()
3) Add documentation, examples and vignettes.
