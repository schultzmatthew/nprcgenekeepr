## Resubmission
This is a resubmission. In this version I have:

* Responded to all requests provided by the reviewer
  -  Reduced the time required for unit test from over 12 minutes to 21.6 
     seconds by skipping those test dependent on schochastic creation of 
     simulated pedigrees and breeding groups when not running on my system.
  -  Reduced the time to run examples and create vignettes by reducing the 
     number of stochastic modeling iterations by orders of magnitude without 
     reducing the examples provided for user-facing functions.
  -  Checking (--as-cran --run-donttest) Duration: 2m 21.8s on my system.
  -  The files with the Rd-tag of \arguments missing do not take arguments.
  -  Corrected private referencing (:::) for exported functions.
  -  Exported all functions used in examples to remove private referencing 
     (:::).
  -  Removed all single quotes on names, abbreviations, initialisms, and,
     acronyms.
  -  The phrase Electronic Health Records (EHR) is the name of a module within
     LabKey, which this software can use as a source of pedigree information
     so the capitalization is appropriate.
  -  The words suspected of being misspelled (EHR, Roboin, and kinships) are
     an initialism of a module in LabKey, a proper name, and a genetic term.
     
## Test environments
* local OS X install, R 3.6.3
* ubuntu 14.04 (on travis-ci), R 3.6.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

* There are currently no downstream dependencies for this package.

