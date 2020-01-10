## Resubmission

* Response to request for fixing ERROR on Solaris,
  https://cran.r-project.org/web/checks/check_results_publipha.html
* Removed every instance of `pow` from C++ code. Now passes CHECK on Solaris.

## Test environments
* local Windows 10, R version 3.6.1
* Ubuntu 16.04 (on Travis-CI), R version 3.6.1, R-devel.
* macOS 10.13 (on Travis-CI), R version 3.6.1
* Oracle Solaris 10, x86, 32 bit, (on R-hub) R-patched

## R CMD check results
There were no ERRORs or WARNINGs. Two NOTEs: One about makefile and one about
line endings. Both are due to rstan.

## Reverse dependencies
There are currently no reverse dependencies for this package.
