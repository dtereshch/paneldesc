## Test environments

* win-builder (R Under development, Windows Server 2022 x64) – OK
* R-hub (Windows Server 2022 x64, R-devel) – OK
* R-hub (macOS Tahoe 26.4, aarch64, R-devel) – OK
* R-hub (Ubuntu 22.04.5 LTS, x86_64, R-devel) – OK
* R-hub (Fedora Linux 42, x86_64, R-devel) – OK

## R CMD check results

0 errors | 0 warnings | 0 notes

All checks passed on all platforms.

## Submission notes

This is a minor release (0.2.0) with the following changes:

### New features
* Added six new functions: `make_balanced()`, `make_wide()`, `make_long()`, 
  `make_demeaned()`, `plot_demeaned()`, and `add_means()`.
* Added a new vignette: *"Panel Data Processing with paneldesc"*.

### Enhancements
* Updated the built-in `production` dataset with a time‑invariant variable, `region`.
* Revised `make_panel()`: its balancing logic has been moved to the new 
  `make_balanced()` function, and the output is now sorted by individual and time.
* Modified `describe_incomplete()` to return an empty data frame when no 
  incomplete units are detected.
* Updated the *"getting-started"* vignette: adjusted figure sizes, added a 
  demonstration of `plot_demeaned()`, and refined the accompanying text.

### Documentation
* Minor improvements to documentation for existing functions.