# heatwaveR 0.5.4 (2025-04-10)

- A lot of small fiddly tweaks to meet new CRAN documentation standards
- Added the scientific citations of the package for 2023 and 2024

# heatwaveR 0.5.3.9005 (2025-01-12)

- Minor tweak to `event_line()` to keep up with changes to __`ggplot2`__

# heatwaveR 0.5.3.9004 (2024-09-25)

- Updated OISST vignette to account for changes in the behaviour of __`tidync`__ functions

# heatwaveR 0.5.3.9003 (2024-08-30)

- Minor tweak to `event_line()` to match new __`ggplot2`__ syntax

# heatwaveR 0.5.3.9002 (2024-07-07)

- Changed moderate category calculation from '>' to '>='

# heatwaveR 0.5.3.9001 (2024-04-22)

- Addressing bugs in the calculation of hourly values not ending on even hour steps

# heatwaveR 0.5.3.9000 (2024-04-09)

- Updated URL for MHW tutorial

# heatwaveR 0.5.3.9000 (2024-03-27)

- Basic hourly functionality added to `ts2clm()`
- Tests added for `detect_event()`
- Minor tweak to `exceedance()` to work with hourly data

# heatwaveR 0.5.2.9009 (2024-01-23)

- More tweaks to return behaviour of `exceedance()`

# heatwaveR 0.5.2.9008 (2024-01-22)

-  Corrected return behaviour of `exceedance()` to keep backwards compatibility

# heatwaveR 0.5.2.9007 (2024-01-20)

-  Fixed a bug in `proto_event()` when an event lasts for the entire time series
-  `exceedance()` now correctly returns extra columns in the original data.frame
-  Corrected rare edge case in `detect_event()` and `exceedance()` where if a single MCS was detected the returned threshold columns would be negative values

# heatwaveR 0.5.2.9006 (2023-12-11)

-  Updated NOAA OISST vignette to account for a change they made to the data query process

# heatwaveR 0.5.2.9006 (2023-12-05)

-  Removed cpp function `seqDates()` from package

# heatwaveR 0.5.2.9005 (2023-11-11)

-  Removed `category` functionality from `detect_event3()` to keep it more streamlined

# heatwaveR 0.5.2.9004 (2023-11-06)

-   Minor tweaks to new functions and CodeCov testing implemented

# heatwaveR 0.5.2.9003 (2023-11-05)

-   Created `detect_event3()` and `proto_event3()` with a new code base to benefit
    from the speed gains due to data.table internals
-   Output results (climatologies and metrics) as data.table
-   Updated and improved documentation

# heatwaveR 0.5.2.9002 (2023-10-31)

-   Rolled back overwrite of `ts2clm()` with `ts2clm3()` for now
-   Set default behaviour of `detect_event()` to return a data.frame, rather than a data.table
-   A new argument was introduced for most top level functions: `returnDF` with the default behaviour of `TRUE`
  -   This explicitly ensures that the functions return data.frames
  -   If `returnDF == FALSE` the functions will return data.tables
-   Removed __`plyr`__ and __`grid`__ dependencies from package
-   Added the package development script to the root directory

# heatwaveR 0.5.2.9001 (2023-10-30)

-   Removed `robust` argument from `ts2clm()` that was deprecated years ago
-   Replaced `ts2clm()` with `ts2clm3()` throughout package
-   Removed __`tibble`__ dependency

# heatwaveR 0.5.2.9000 (2023-10-29)

-   Fully data.table compliant `ts2clm3()`, a drop-in replacement for `ts2clm()`
-   `make_whole_fast()`, `na_interp()`, `clim_spread()`, and `smooth_percentile()` included within the main function
-   Significantly faster climatology calculation
-   Correct error with climatological variance calculation

# heatwaveR 0.5.1.9000 (2023-10-25)

-   Removing dependencies and speeding up code
-   Removed __`lubridate`__ and __`dplyr`__ dependencies

# heatwaveR 0.5.0.9003 (2023-08-31)

-   Added links to __`heatwave3`__ package on home page and gridded detection vignette.

# heatwaveR 0.5.0.9003 (2023-07-31)

-   Addressed CodeCov and created stable version for CRAN

# heatwaveR 0.5.0.9003 (2023-07-25)

-   Changed behaviour of `category()` to address edge case in polar MCS
-   Corrected bug in `category()` that did not change the 'V Ice' category in the climatology output
-   Also a bug that didn't return all of a users columns that are not part of the normal `category()` output
-   And a bug that allowed users to unintentionally assign 'V Ice' category to MHWs

# heatwaveR 0.5.0.9002 (2023-05-02)

-   Addressed extreme edge case in `category()` when no Moderate I MCS are detected, just higher categories

# heatwaveR 0.5.0.9001 (2023-04-17)

-   Improvements to main pipeline to ensure `lat` column is passed forward for correct category seasons 

# heatwaveR 0.5.0.9000 (2023-03-30)

-   Bug hunting in the build-up to v1.0.0
-   Addressed issue of y column not passing through correctly from `detect_event()` to `category()` when called internally
-   Changed documentation for `category()` to more accurately match the output of `climatology = TRUE`
-   Looked into behaviour of `protoEvents` in `detect_event()`
-   Added `lat_col` argument to `category()` to detect if time series are in N or S hemisphere

# heatwaveR 0.5.0 (2023-03-20)

-   Major update in preparation of removing all dependencies and releasing v1.0.0
-   Also necessary due to cryptic CRAN error

# heatwaveR 0.4.6.9004 (2023-03-13)

-   Removed all dependencies from `event_line()` and `block_average()` except **`plyr`**
-   Added `line_colours` argument to `event_line()`, allowing users to choose colours of line geoms

# heatwaveR 0.4.6.9004 (2023-03-11)

-   Removed all dependencies from `category()` and `exceedance()` except for **`plyr`**

# heatwaveR 0.4.6.9004 (2023-03-07)

-   Starting to remove as many dependencies as possible
-   Started by removing all but `**plyr**` dependency from `proto_event()` and `detect_event()`

# heatwaveR 0.4.6.9003 (2023-01-16)

-   Updated SST data to end of 2022
-   Changed behaviour of `lolli_plot()` and `event_line()` to now use tidyeval
  -   Note that this is a backwards compatibility breaking change that was required due to changes in **`ggplot2 v3.0.0`**

# heatwaveR 0.4.6.9002 (2023-01-09)

-   Updated citations page for all references to `heatwaveR` from 2022
-   Added package names to all functions used in the downloading OISST vignette
-   Added the `MCSice` argument to `category()`, which allows users to directly classify a MCS with a threshold below -1.7°C as a category 'V Ice' event _sensu_ Schlegel et al. (2021; Marine cold-spells)

# heatwaveR 0.4.6.9001 (2022-06-23)

-   Added two new vignettes 
    - Alternatives to downloading NOAA OISST data
    - Trend and break point analyses for MHW metrics

# heatwaveR 0.4.6.9000 (2022-01-17)

-   Updated SST time series to 2021-12-31

# heatwaveR 0.4.6 (2021-10-26)

-   Updated SST time series to 2020-12-31

# heatwaveR 0.4.6 (2021-10-25)

-   Explicitly loading **`Rcpp`** due to a change in the behaviour of the package causing ERRORs on CRAN checks

# heatwaveR 0.4.5.9002 (2021-09-21)

-   Updated the MCS colour palette throughout the package
    - The new colour palette is very similar, but with improved contrast
-   Made a minor but important change to `category()` so that categories are based on temperature greater (lesser) than the thresholds and not greater (lesser) than or equal to

# heatwaveR 0.4.5.9001 (2021-08-25)

-   Updated the visualisation vignette to now include the code necessary to get `geom_flame()` to work with **`plotly`** now that it is no longer directly supported

# heatwaveR 0.4.5.9001 (2021-03-17)

-   Improved error trapping for `ts2clm()`, `detect_event()`, and `exceedance()`

# heatwaveR 0.4.5.9000 (2021-02-10)

-   Added more citations of the package

# heatwaveR 0.4.5.9000 (2021-01-31)

-   Added a `categories` argument to `detect_event()`
    -   This allows one to determine the categories of events directly, rather than as a second step using the `category()` function

# heatwaveR 0.4.5 (2021-01-23)

-   Created a new vignette that shows how to save gridded MHW results to a NetCDF file

# heatwaveR 0.4.5 (2021-01-07)

-   Pushed new major version to CRAN

# heatwaveR 0.4.4.9006 (2021-01-06)

-   Some minor tweaks to satisfy CRAN

# heatwaveR 0.4.4.9005 (2020-12-30)

-   Pushing to GitHub actions now triggers codecov correctly

# heatwaveR 0.4.4.9005 (2020-12-22)

-   `exceedance` now outputs a one row all NA data.frame when there are no events in exceedance of the threshold
-   The previous version would return an error if no exceedances were detected, making it generally unusable in gridded data
-   All **`plotly`** dependencies were removed due to the orphaning of the package
    - The code to allow `geom_flame` to work with **`plotly`** may be found here: https://github.com/robwschlegel/MHWapp/blob/master/shiny/functions.R

# heatwaveR 0.4.4.9004 (2020-12-16)

-   Moved away from Travis CI to GitHub actions 

# heatwaveR 0.4.4.9004 (2020-11-18)

-   Fixed bug in `proto_event` that caused `joinAcrossGaps` argument to flag the first n days of a time series as part of an event

# heatwaveR 0.4.4.9003 (2020-11-13)

-   Minor fixes for passing build

# heatwaveR 0.4.4.9002 (2020-11-06)

-   Remove the last offending trace of **`zoo`** from `make_whole`.
-   Fix miscellaneous typos and formatting inconsistencies.

# heatwaveR 0.4.4.9001 (2020-10-31)

-   Added `MCScorrect` argument to `catgegory` function that allows the user to bound the bottom threshold for MCS categories to -1.8C

# heatwaveR 0.4.4.9000 (2020-07-30)

-   dplyr v1.0.0 no longer handles empty data.frames the same

    -   It now forces them to logical type, so empty data.frames no longer unnest with normal results
    -   This was fixed by having `detect_event` and `category` return single row NA data.frames instead

-   Thinking about phasing out **`data.table`**

# heatwaveR 0.4.4 (2020-06-27)

-   Released v0.4.4 to CRAN

# heatwaveR 0.4.3.9001 (2020-06-26)

-   Re-wrote OISST and gridded detection vignettes after finding an ERDDAP server that hosts the NOAA OISST v2.1 data.
-   Updated the packaged time series to now use v2.1 data from 1982 to 2019

# heatwaveR 0.4.3.9000 (2020-06-25)

-   Minor bug fix in gridded detection vignette

# heatwaveR 0.4.3.9000 (2020-06-15)

-   `category()` now works with MCS generated by `detect_event()`
-   codecov pushed back up to 100%

# heatwaveR 0.4.3 (2020-06-12)

-   Release pushed to CRAN in light of changes to **`data.table`** and **`dplyr`**
-   Waiting until NOAA OISST data area available on an ERDDAP server before updating them

# heatwaveR 0.4.2.9005 (2020-06-06)

-   Updated the gridded data vignette to work with OISST v2.1

# heatwaveR 0.4.2.9005 (2020-06-03)

-   Ensured that newly released **`dplyr`** v1.0.0 did not introduce any bugs
-   Updated vignettes to now work with v2.1 of the OISST data

# heatwaveR 0.4.2.9004 (2020-05-15)

-   Fixed bug in `detect_event()` caused by **`data.table`**

    -   **`data.table`** no longer allows rounding of NA values

# heatwaveR 0.4.2.9003 (2020-05-08)

-   Fixed bug in `event_line()` caused by **`data.table`**

# heatwaveR 0.4.2.9002 (2020-05-05)

-   Cleaned up the CITATION file

# heatwaveR 0.4.2.9002 (2020-03-23)

-   Improvement to legend appearance for `event_line()`
-   Improvement to plotting in category vignette

# heatwaveR 0.4.2.9001 (2020-02-24)

-   Minor spelling error fixes
-   Minor tweak to `make_whole_fast()`

# heatwaveR 0.4.2.9001 (2020-02-13)

-   Added a new citation that used the `heatwaveR` package

# heatwaveR 0.4.2.9001 (2020-02-05)

-   Minor internal tweak to exceedance function

# heatwaveR 0.4.2.9001 (2020-01-13)

-   Added one small test to get codecov back up to 100%

# heatwaveR 0.4.2.9001 (2020-01-08)

-   Beginning to phase out **`dplyr`** and **`tibble`** dependencies
-   Replacing some **`dplyr`** functions with **`plyr`** as they appear to be slightly faster and also don't have the same multicore issues that more recent **`dplyr`** developments have created
-   Added `roundRes` argument to `detect_event()` to allow users to decide the rounding precision of the returned results

# heatwaveR 0.4.2 (2019-12-10)

-   Minor change to `ts2clm()` that does not alter any functionality

# heatwaveR 0.4.2 (2019-11-29)

-   Publishing new stable version to CRAN due to removal of **`tidyverse`** from the list of suggested packages and the important bug fixes to `category()`

# heatwaveR 0.4.2 (2019-11-26)

-   Fixed bug in `category()` that incorrectly returned the difference between `seas` and `thresh` as the daily intensity value, rather than the actual temperature anomaly above `seas`

    -   Also fixed the labelling of days below `thresh` as `Moderate`, they are now `NA`

-   `category()` now adds lower case letters to the ends of event names if there were multiple large events within the same year

    -   This may cause backwards compatibility issues in very rare cases, but is a necessary change

-   Added `roundVal` argument to `category()` to allow users to decide the rounding precision of the returned results

# heatwaveR 0.4.1.9004 (2019-11-23)

-   Tripled the resolution of the logo in anticipation of creating hex stickers

# heatwaveR 0.4.1.9004 (2019-11-19)

-   Removed **`tidyverse`** from the list of suggested packages

# heatwaveR 0.4.1.9003 (2019-11-03)

-   The potentially backward compatibility breaking design change to have empty data.frames returned as one row of `NA` values was not as rare of a problem as first thought
-   While still respecting the need to not use `1:n` vector creation, `detect_event()` and `category()` now produce empty data.frames and not one row `NA` dataframe
-   This should ensure continued backwards compatibility

# heatwaveR 0.4.1.9002 (2019-11-03)

-   A bug was discovered where the `ts2clm()` function does not first check that the time series being fed to it is in correct chronological order

    -   This was fixed by ordering the data being fed to the function before beginning of the further calculations

# heatwaveR 0.4.1.9001 (2019-10-30)

-   An update to `data.table` sometime in August, 2019 reduced the packages tolerance for code with `1:n` in it

    -   As this is a potential memory weakness, when this creates an empty vector in multicore calculations it causes the entire run to fall over
    -   Therefore, all use of `1:n` has been replaced with `seq_len(n)`
    -   This changed the list outputs when no MHWs are detected to be one row of NA values, rather than an empty list
    -   This may cause backward compatibility issues, but is extremely unlikely

# heatwaveR 0.4.1.9000 (2019-10-26)

-   An update to the `ggplot2` package some time back in August, 2019 changed slightly how Geoms work internally

    -   This allowed `geom_lolli()` to fall over in very rare circumstances so this issue has been addressed

# heatwaveR 0.4.1 (2019-09-09)

-   Updated the `OISST Preparation` vignette to match the improvements to the `rerddap` package

-   Also updated the vignette to be more clear to contact NOAA about data retrieval issues

-   Added `season` argument to `category()`, which allows the user to specify which season of the MHWs they are interested in:

    -   'range' - The beginning and ending season of the MHW
    -   'start' - The season during the start of the MHW
    -   'peak' - The season during the peak of the MHW
    -   'end' - The season during the end of the MHW

# heatwaveR 0.4.0.9000 (2019-07-22)

-   Updated NOAA OISST data downloading tutorial to account for minor changes to NOAA ERDDAP servers

# heatwaveR 0.4.0 (2019-07-10)

-   Introduced the `Algiers` time series for examples on using multiple thresholds for atmospheric data
-   Fixed a bug in `event_line()`

# heatwaveR 0.4.0 (2019-07-09)

-   Changed the `data.table` and `ggplot2` dependencies to imports
-   `heatwaveR` is now dependency free

# heatwaveR 0.3.6.9004 (2019-05-29)

-   Added the ability to choose the number of digits rounded to for the output of `ts2clm()`

# heatwaveR 0.3.6.9003 (2019-02-11)

-   Updated the end date for three time series packaged with **`heatwaveR`** to 2018-12-31

# heatwaveR 0.3.6.9002 (2019-02-11)

-   Added `n` and `n_gap` arguments to `geom_flame()` to allow proper screening of heatspikes
-   These additions also propagate through to `geom2trace.GeomFlame`

# heatwaveR 0.3.6.9001 (2019-01-28)

-   Added some additional options to `event_line()` to allow users to manipulate the output more without having to get into the source code.

# heatwaveR 0.3.6.9000 (2019-01-23)

-   Added S3 object `geom2trace.GeomFlame` that now allows `geom_flame()` to be converted to an interactive plotly object via `plotly::ggplotly()`

# heatwaveR 0.3.6 (2019-01-16)

-   Updated Zenodo badge to reflect the upload of v0.3.6

# heatwaveR 0.3.6 (2019-01-16)

-   Finished Alternative Thresholds (Complex Climatologies) vignette

-   Bumped code coverage back up to 100%

-   AN important potential backwards compatibility breaking change is that by default `ts2clm()` and `exceedance()` will no longer produce a `var` column

    -   The argument `var = TRUE` may be given to produce this column

-   Submitted v0.3.6 to CRAN

# heatwaveR 0.3.5.9006 (2019-01-15)

-   Began editing Alternative (Complex) Climatology vignette
-   Changed the default argument for `exceedance(maxPadLength)` from `3` to `FALSE` to match the new default for `ts2clm()`
-   Changed the default behaviour of `event_line()` to no longer require the user to provide `start_date` and `end_date` arguments

# heatwaveR 0.3.5.9005 (2019-01-03)

-   Corrected some typos in the gridded event detection vignette

# heatwaveR 0.3.5.9004 (2019-01-02)

-   First update of 2019
-   Added 'protoEvents' argument to `detect_events.R()` for returning the proto events rather than a table for the event metrics

# heatwaveR 0.3.5.9003 (2018-12-26)

-   Boxing day update

# heatwaveR 0.3.5.9003 (2018-12-21)

-   Updated text for the OISST preparation vignette
-   Updated text for the gridded event detection vignette

# heatwaveR 0.3.5.9003 (2018-12-19)

-   Updated text on the landing page to better reflect the updates that have been made over the past few months
-   Fixed typo in `event_line()` output
-   Updated text for the detection and visualisation vignette
-   Updated text for the exceedance vignette
-   Updated text for the categories vignette
-   Updated text for the OISST preparation vignette

# heatwaveR 0.3.5.9003 (2018-12-18)

-   Changed time series checking behaviour of `exceedance()` so that it is the same as `ts2clm()`

# heatwaveR 0.3.5.9002 (2018-12-12)

-   Fixed bug in `event_line()` that caused it to graph events outside of the `spread` range
-   Expanded testing back up to 100%

# heatwaveR 0.3.5.9001 (2018-12-06)

-   Fixed bug caused by R not liking dates older than 1970-01-01
-   Changed `maxPadLength` behaviour in `ts2clm()` to match the Python default settings

# heatwaveR 0.3.5.9000 (2018-12-05)

-   Added 'duration_max' to `block_average()` output
-   Resumed correct version numbering

# heatwaveR 0.3.5 (2018-12-03)

-   Increased functionality of `block_average()`

# heatwaveR 0.3.5 (2018-11-01)

-   Updated one figure in a vignette

# heatwaveR 0.3.5 (2018-10-26)

-   `clim_calc()` reinstated to allow for calculation of clims with missing data
-   `var` calculations reinstated for documentation issues

# heatwaveR 0.3.5 (2018-10-20)

-   `ts2clm()` no longer calls `clim_calc()`, but `clim_calc_cpp()` only
-   `smooth_percentile()` no longer provides option to create variance climatology (the need to no longer create var seemed to not be fully implemented in 0.3.4)

# heatwaveR 0.3.4 (2018-10-19)

-   `ts2clm()` no longer calculates variance column by default
-   `make_whole()` has been deprecated in favour of `make_whole_fast()`

# heatwaveR 0.3.4 (2018-10-17)

-   All major functions now produce results only up to the fourth decimal place

# heatwaveR 0.3.4 (2018-10-16)

-   Clarified some information on the basic detection vignette
-   Corrected a link that went to the wrong page

# heatwaveR 0.3.4 (2018-10-03)

-   Changed error handling in `proto_event()` to return no events than to stop message with an error.
-   This change was picked up by `detect_event()` without any required changes
-   `category()` required a bit of cajoling to also output a blank dataframe

# heatwaveR 0.3.4 (2018-10-01)

-   Minor tweak to `make_whole_fast()` to provide a cleaner internal output

# heatwaveR 0.3.4 (2018-09-28)

-   Removed several unnecessary columns from category climatology output

# heatwaveR 0.3.4 (2018-09-27)

-   Fixed bug in `ts2clm()` that prevented calculation of clims with large contiguous missing periods of data (e.g. ice coverage).
-   Added argument to `category()` that allows one to have the function also output the day-to-day (long) category values, rather than just the summary (wide) output.
-   Added lon/lat values to documentation for built-in time series

# heatwaveR 0.3.3 (2018-08-23)

-   Added CITATION file so that package citation is now set to JOSS article

# heatwaveR 0.3.3 (2018-07-31)

-   Added Zenodo DOI badge
-   JOSS review process complete
-   Added JOSS DOI badge

# heatwaveR 0.3.3 (2018-07-25)

-   BUG FIX: corrected issue with `clim_calc_cpp` not being able to calculate clims from baselines not beginning and ending on the Julian year by making `clim_spread` plug the gaps beforehand with row-wise means.
-   Rebuilt pkgdown site to reflect version increase
-   v0.3.3 submitted to CRAN

# heatwaveR 0.3.2 (2018-07-23)

-   Edits suggested through JOSS review

# heatwaveR 0.3.2 (2018-07-12)

-   Remove unneeded copies of data from functions to improve memory-use efficiency.

# heatwaveR 0.3.1 (2018-07-10)

-   BUG FIX: corrected issue with `make_whole_fast` which did not create a whole, complete time series (i.e. missing dates were still present); the missing dates caused `clim_calc_cpp` to fail

# heatwaveR 0.3.0 (2018-06-22)

-   Re-submitted to CRAN in anticipation of **`ggplot2`** changes
-   `proto_event` now handles all event calculations 'in house'
-   This allows `detect_event` to now be given a theoretically limitless number of thresholds

# heatwaveR 0.2.7.9003 (2018-06-08)

-   Logic catch for `lolli_plot` being asked to highlight more events than are present
-   New vignette that looks at calculating more complex climatologies

# heatwaveR 0.2.7.9002 (2018-06-07)

-   Tweak to `ts2clm`

# heatwaveR 0.2.7.9001 (2018-06-05)

-   Tweaks to `detect_event` and `exceedance`

# heatwaveR 0.2.7.9000 (2018-06-02)

-   Unused Rcpp code removed from master branch
-   Logo changed slightly
-   Codecov back up to 100%
-   Addressed one testthat issue that was causing the OSX CRAN build to fail
-   Added CRAN link to pkgdown site
-   Added Bug Report link to pkgdown site

# heatwaveR 0.2.7 (2018-05-30)

-   Accepted on CRAN

# heatwaveR 0.2.7 (2018-05-28)

-   Submitted to CRAN

# heatwaveR 0.2.6.9002 (2018-05-25)

-   Fixes to `make_whole` and testing
-   Fixes to `block_average`
-   No longer exporting `make_whole` and `make_whole_fast`
-   No longer uses zoo for time series NA handling--made custom function to replace it
-   Additional speed improvements
-   Repair testthat tests

# heatwaveR 0.2.6.9001 (2018-05-24)

-   Moved all vignettes relating to the upcoming MHW_detection paper to that repo
-   This helps to unclutter this repo as it should be primarily kept for package content

# heatwaveR 0.2.6.9000 (2018-05-23)

-   Added vignette that shows how tweaking arguments for detect changes the outputs between languages and how those outputs may differ
-   Changed output of `detect_event` to better match Python version
-   Corrections to testthat to match changes to `detect_event` output

# heatwaveR 0.2.5.9003 (2018-05-22)

-   Added C++ function, `clim_calc_ccp()` for faster climatology calculations; speed of climatology calculation comes down from 50.6 ms in R to 3.4 ms in C++ on my MacBook Pro (15-inch, 2017) 2.9 GHz Intel Core i7 16 GB RAM computer

# heatwaveR 0.2.4.9003 (2018-05-21)

-   Updated testthat for `lolli_plot()` and `event_line()`
-   Updated testthat for `ts2clm()`
-   Updated testthat for `detect_event()`

# heatwaveR 0.2.4.9002 (2018-05-17)

-   Take advantage of C++ speed enhancement in `smooth_percentile()` by using RcppRoll
-   Update testthat accordingly

# heatwaveR 0.2.3.9002 (2018-05-17)

-   Basic R vs Python vignette finished

# heatwaveR 0.2.3.9001 (2018-05-16)

-   Minor fix to testthat
-   codecov up to 100%
-   Fix to `geom_lolli()` n argument
-   Fix to `lolli_plot()` y-axis range
-   Minor fix to `make_whole()`
-   Skeleton of R vs Python vignette added

# heatwaveR 0.2.3.9000 (2018-05-16)

-   Major speed-up in the climatology creation function. `clim_spread()` now returns a matrix, not a data frame. This makes the loop in `clim_calc()` much faster. In testing with the sst_WA data, it leads to a 3.7 fold speed improvement (520 ms down to 140 ms).
-   Speed-up of `make_whole()` (60 ms down to 40 ms)

# heatwaveR 0.2.2.9000 (2018-05-15)

-   Removed all instances of `detect()` in favour of the new pipeline
-   Updated `exceedance()` to utilise the internal functions
-   Updated object names in `block_average()`

# heatwaveR 0.2.1.9001 (2018-05-15)

-   Micro edits to documentation
-   Testing for all exported and internal functions brought up to speed
-   Ensuring that new `ts2clm()` and `detect_event()` pipeline returns the same results as the old `make_whole()` and `detect()` pipeline

# heatwaveR 0.2.1.9000 (2018-05-14)

-   Phasing in identical names as in the python version
-   `detect_event()` now passing checks
-   Must still test for MCSs
-   The old `detect()` function was unpacked and simplified. Internal code is now in new functions, most of which will not be seen by the user. They are `make_whole()` `proto_event()`, `clim_calc()`, `smooth_percentile()`, `clim_spread()`, and `ts2clm()`
-   `ts2clm()` used instead of `detect_clim()`
-   Climatologies can now be calculated independently of the detect functionality
-   `exceedance()` function testthat checks updated to account for change in variable naming

# heatwaveR 0.2.0.9000 (2018-05-11)

-   `detect()` has now been broken into `detect_clim()` and `detect_event()`
-   These now also rely on internal functions
-   The purpose of this is to create a family of functions that provide different options
-   New vignette on making short climatologies.

# heatwaveR 0.1.0.9000 (2018-05-10)

-   One may now provide alternative baselines and climatologies to `detect()`

# heatwaveR 0.0.7.9001 (2018-05-05)

-   Testing for `category()`
-   Testing for `block_average()`
-   Testing for `detect()`
-   Testing `exceedance()`
-   Removed default `threshold` for `exceedance()`
-   Tweaks to `exceedance()` error messages
-   Testing for `event_line()`
-   Testing for `lolli_plot()`
-   Tweaks to `lolli_plot()` error messages

# heatwaveR 0.0.7.9000 (2018-05-04)

-   New `category()` function returns the category results for events
-   Still requires testing and improved event naming scheme

# heatwaveR 0.0.6.9000 (2018-05-03)

-   Minor touch up to examples in `geoms.R`
-   First draft of `heatwaveR` hex logo added to site
-   Added `category` option to `event_line()`
-   Simplifications and consistency checks to `detect()`
-   Some writing on Baselines and climatology vignette

# heatwaveR 0.0.5.9001 (2018-05-02)

-   Fix to `event_line()` not plotting MCSs correctly
-   Fix error with smooth_percentile and smooth_percentile_width descriptions that were interchanged in `detect()`
-   Simplify initial lines of leap year calculations (remove redundant code)
-   change from `raster::quantile()` to `stats::quantile()`

# heatwaveR 0.0.5.9000 (2018-04-29)

-   Add option to use a custom baseline to `detect()` as requested by Maxime Marin (), The University of Tasmania (IMAS) -- CSIRO (O&A), and which is present in the python version of the package

# heatwaveR 0.0.4.9000 (2018-04-28)

-   Remove restriction to require full years for start/end points of climatology calculations in `detect()`
-   Documentation updated accordingly
-   Vignette on OISST data processing added

# heatwaveR 0.0.3.9000 (2018-04-26)

-   Removed rlang dependency
-   Touch-up to `block_average()`
-   Tested `make_whole()`
-   Basic testing for other functions

# heatwaveR 0.0.2.9000 (2018-04-25)

-   Established theme for changelog
-   Synced ganalytics
-   Fixed `event_line()` to acknowledge column names other than `t` and `temp`
-   Fixed `lolli_plot()` to use underlying `geom_lolli()`
-   Search bar now live
-   Removed all but one use of plyr code

# heatwaveR 0.0.1.9000 (2018-04-24)

-   Added a `NEWS.md` file to track changes to the package.
-   Cloned **`RmarineHeatWaves`** package to this repo
-   First build of **`pkgdown`** site live
