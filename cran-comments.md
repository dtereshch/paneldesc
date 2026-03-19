## Resubmission
This is a resubmission. In response to the CRAN comment, I have:

* Removed the redundant `+ file LICENSE` from the `License` field in DESCRIPTION.
* Deleted the `LICENSE` file, as it is not needed for a standard GPL‑3 license.
* Incremented the version number to 0.1.1.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking for future file timestamps ... NOTE
  unable to verify current time

  This note appears only on my local development machine, presumably due to a system‑specific issue with timestamp verification (locale / time synchronisation).
