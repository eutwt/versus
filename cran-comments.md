## Test environments

* local macOS 14.4.1, R 4.5.1
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note (local) / 1 note (win-builder)

* Local macOS: `checking for future file timestamps ... NOTE`  
  unable to verify current time (expected in sandboxed CI environment)
* Win-builder (devel): `checking CRAN incoming feasibility ... NOTE`  
  Version contains large components (0.3.0.9000) — expected when checking dev builds

## Additional comments

* Current CRAN check results for 0.3.0 show a NOTE about the non-API call `STRING_PTR` in compiled code. The package no longer includes compiled code, so this NOTE should disappear with 0.3.1.
