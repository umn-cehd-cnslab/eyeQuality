## Test environments

* Local Windows 10, R 4.2.1 (x86_64-w64-mingw32/x64), via
  `devtools::check(cran = TRUE)`
* Win-builder (R-devel), via `devtools::check_win_devel()` — submitted
  2026-08-19; results pending by email to the maintainer.

R-hub multi-platform checks (`rhub::rhub_check()`) have not been completed
yet. This requires the R-hub GitHub App to be authorized for the package
repository and a stored GitHub PAT for the R-hub v2 GitHub Actions-based
workflow, neither of which is set up yet; this is planned as a follow-up
step before the actual submission.

## R CMD check results

0 errors | 0 warnings | 1 note

```
checking for future file timestamps ... NOTE
unable to verify current time
```

This NOTE comes from R CMD check's own clock-verification step, which
queries a network time service to confirm the local system clock isn't set
in the future; it reflects the build machine's network access rather than
anything about the package, and is expected to vary (or disappear entirely)
depending on the checking environment.

## Downstream dependencies

This is a new release, so there are no reverse dependencies to check.
