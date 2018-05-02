# heatwaveR 0.0.5.9001 (2018-05-02)

* Fix to `event_line()` not plotting MCSs correctly
* Fix error with smooth_percentile and smooth_percentile_width descriptions
  that were interchanged

# heatwaveR 0.0.5.9000 (2018-04-29)

* Add option to use a custom baseline to `detect()` as requested by
  Maxime Marin (), The University of Tasmania (IMAS) – CSIRO (O&A),
  and which is present in the python version of the package

# heatwaveR 0.0.4.9000 (2018-04-28)

* Remove restriction to require full years for start/end points of
  climatology calculations in `detect()`
* Documentation updated accordingly
* Vignette on OISST data processing added

# heatwaveR 0.0.3.9000 (2018-04-26)

* Removed rlang dependency
* Touch-up to `block_average()`
* Tested `make_whole()`
* Basic testing for other functions

# heatwaveR 0.0.2.9000 (2018-04-25)

* Established theme for changelog
* Synced ganalytics
* Fixed `event_line()` to acknowledge column names other than `t` and `temp`
* Fixed `lolli_plot()` to use underlying `geom_lolli()`
* Search bar now live
* Removed all but one use of plyr code

# heatwaveR 0.0.1.9000 (2018-04-24)

* Added a `NEWS.md` file to track changes to the package.
* Cloned __`RmarineHeatWaves`__ package to this repo
* First build of __`pkgdown`__ site live
