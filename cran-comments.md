## Test environments

* local macOS 14.4.1, R 4.5.1
* win-builder (devel) — submitted via `devtools::check_win_devel()`, results pending

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE  
  unable to verify current time (expected in sandboxed CI environment)

## Additional comments

* Current CRAN check results for 0.3.0 show a NOTE about the non-API call `STRING_PTR` in compiled code. The package no longer includes compiled code, so this NOTE should disappear with 0.3.1.
