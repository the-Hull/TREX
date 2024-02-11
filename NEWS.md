# TREX 1.0.2

* added `NEWS.md`
* changed from Travis CI to Github actions for checks and `pkdown` page
* added "_PACKAGE" definition to package description
* use `inherits()` for class assertion in `tdm_damp()`
* adjusted `tdm_cal.sfd()` to use `vector`, not `data.frame` in `stats::quantile()`
* adjusted `tdm_dt.max()` to account for data gaps in window approaches (e.g., when choosing 5-day window but only 4 days of data in a given period are available)


# TREX 1.0.1

* added allometry functionalities
-

# TREX 1.0.0
* Initial CRAN submission.
