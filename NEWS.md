# paneldesc 0.2.0

## New features
* Added functions: `make_balanced()`, `make_wide()`, `make_long()`, `make_demeaned()`, `plot_demeaned()`, and `add_means()`.
* Added a new vignette: "Panel Data Processing with paneldesc".

## Enhancements
* Updated the built-in `production` dataset with a time-invariant variable, `region`.
* Revised `make_panel()`: balance logic moved to `make_balanced()`; output is now sorted by individual and time.
* Modified `describe_incomplete()` to return an empty data frame when no incomplete units are found.
* Updated the "getting-started" example: adjusted figure sizes and added `plot_demeaned()`.

## Documentation
* Minor improvements to documentation for older functions.

# paneldesc 0.1.1

* Resubmission to CRAN: removed redundant LICENSE file and corrected License field in DESCRIPTION as requested.

# paneldesc 0.1.0

* Initial CRAN submission.
