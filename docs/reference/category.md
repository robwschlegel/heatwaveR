# Calculate the categories of events.

Calculates the categories of MHWs or MCSs produced by
[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
in accordance with the naming scheme proposed in Hobday et al. (2018).

## Usage

``` r
category(
  data,
  y = temp,
  S = TRUE,
  name = "Event",
  climatology = FALSE,
  MCScorrect = FALSE,
  MCSice = FALSE,
  season = "range",
  roundVal = 4,
  lat_col = FALSE
)
```

## Arguments

- data:

  The function receives the full (list) output from the
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
  function.

- y:

  The column containing the measurement variable. If the column name
  differs from the default (i.e. `temp`), specify the name here.

- S:

  This argument informs the function if the data were collected in the
  southern hemisphere (TRUE, default) or the northern hemisphere (FALSE)
  so that it may correctly output the `season` column (see below).

- name:

  If a character string (e.g. "Bohai Sea") is provided here it will be
  used to name the events in the `event_name` column (see below) of the
  output. If no value is provided the default output is "Event".

- climatology:

  The default setting of `FALSE` will tell this function to output only
  the summary (wide) results for the individual events as seen in Hobday
  et al. (2018). If set to `TRUE`, this function will return a list of
  two dataframes. The first dataframe `climatology`, contains similar
  information as found in
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md),
  with the addition of the daily intensity (anomaly above seasonal doy
  threshold) and category values, but only reports the days on which an
  event was detected. The second dataframe, `event`, is the summary
  results that this function produces by default.

- MCScorrect:

  When calculating marine cold-spells (MCSs) it may occur in some areas
  that the bottom thresholds for the more intense categories will be
  below -1.8C, this is physically impossible on Earth, so if one wants
  to correct the bottom thresholds to not be able to exceed -1.8C, set
  this argument to TRUE (default is FALSE).

- MCSice:

  Sensu Schlegel et al. (2021; Marine cold-spells), it is advisable to
  classify a MCS with an event threshold below -1.7°C as a 'V Ice'
  category event.

- season:

  This argument allows the user to decide how the season(s) of
  occurrence for the MHWs are labelled. The default setting of `"range"`
  will return the range of seasons over which the MHW occurred, as seen
  in Hobday et al. (2018). One may chose to rather have this function
  return only the season during the "start", "peak", or "end" of the MHW
  by giving the corresponding character vector.

- roundVal:

  This argument allows the user to choose how many decimal places the
  outputs will be rounded to. Default is 4. To prevent rounding set
  `roundClm = FALSE`. This argument may only be given numeric values or
  FALSE.

- lat_col:

  The user may set `lat_col = TRUE` to detect columns named first 'lat',
  then 'latitude', and use the numeric decimal degree values therein to
  determine the correct seasons for events. Note that this will override
  the `S` argument. Meaning that if the given/detected latitude column
  has negative values, `S` will automatically be set to `TRUE` and vice
  versa. Also note that if multiple different latitude values are
  detected this will intentionally cause an error because the
  `category()` function is not meant to be run on more than one time
  series at once. If latitude is exactly 0, it will be classified as
  Northern Hemisphere.

## Value

The function will return a data.frame with results similar to those seen
in Table 2 of Hobday et al. (2018). This provides the information
necessary to appraise the extent of the events in the output of
[`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
based on the category ranking scale. The category thresholds are
calculated based on the difference between the given seasonal
climatology and threshold climatology. The four category levels are then
the difference multiplied by the category level.

The definitions for the default output columns are as follows:

- t:

  The column containing the daily date values.

- event_no:

  The numeric event number label.

- intensity:

  The daily exceedance (default is degrees C) above the seasonal
  climatology.

- category:

  The category classification per day.

- event_no:

  The number of the event as determined by
  [`detect_event`](https://robwschlegel.github.io/heatwaveR/reference/detect_event.md)
  to allow for joining between the outputs.

- event_name:

  The name of the event. Generated from the `name` value provided and
  the year of the `peak_date` (see following) of the event. If no `name`
  value is provided the default "Event" is used. As proposed in Hobday
  et al. (2018), `Moderate` events are not given a name so as to prevent
  multiple repeat names within the same year. If two or more events
  ranked greater than Moderate are reported within the same year, they
  will be differentiated with the addition of a trailing letter (e.g.
  Event 2001a, Event 2001b).

- peak_date:

  The date (day) on which the maximum intensity of the event was
  recorded.

- category:

  The maximum category threshold reached/exceeded by the event.

- i_max:

  The maximum intensity of the event above the threshold value.

- duration:

  The total duration (days) of the event. Note that this includes any
  possible days when the measurement value `y` may have dropped below
  the threshold value. Therefore, the proportion of the event duration
  (days) spent above certain thresholds may not add up to 100% (see
  following four items).

- p_moderate:

  The proportion of the total duration (days) spent at or above the
  first threshold, but below any further thresholds.

- p_strong:

  The proportion of the total duration (days) spent at or above the
  second threshold, but below any further thresholds.

- p_severe:

  The proportion of the total duration (days) spent at or above the
  third threshold, but below the fourth threshold.

- p_extreme:

  The proportion of the total duration (days) spent at or above the
  fourth and final threshold.

- season:

  The season(s) during which the event occurred. If the event occurred
  across two seasons this will be displayed as e.g. "Winter/Spring".
  Across three seasons as e.g. "Winter-Summer". Events lasting across
  four or more seasons are listed as "Year-round". December (June) is
  used here as the start of Austral (Boreal) summer. If "start", "peak",
  or "end" was given to the `season` argument then only the one season
  during that chosen period will be given.

If `climatology = TRUE`, this function will output a list of two
dataframes. The first dataframe, `climatology`, will contain the
following columns:

- t:

  The column containing the daily date values.

- event_no:

  The numeric event number label.

- intensity:

  The daily exceedance (default is degrees C) above the seasonal
  climatology.

- category:

  The category classification per day.

The second dataframe, `event`, contains the default output of this
function, as detailed above.

## Details

An explanation for the categories is as follows:

- I Moderate-:

  Events that have been detected, but with a maximum intensity that does
  not double the distance between the seasonal climatology and the
  threshold value.

- II Strong-:

  Events with a maximum intensity that doubles the distance from the
  seasonal climatology and the threshold, but do not triple it.

- III Severe-:

  Events that triple the aforementioned distance, but do not quadruple
  it.

- IV Extreme-:

  Events with a maximum intensity that is four times or greater than the
  aforementioned distance.

- V Ice-:

  If `MCSice = TRUE`, a MCS with an event threshold below -1.7°C will be
  classified here.

## References

Hobday et al. (2018). Categorizing and Naming Marine Heatwaves.
Oceanography 31(2).

Schlegel et al. (2021). Marine cold-spells. Progress in Oceanography
198(102684).

## Author

Robert W. Schlegel

## Examples

``` r
res_WA <- detect_event(ts2clm(sst_WA,
                       climatologyPeriod = c("1983-01-01", "2012-12-31")))
# Note that the name argument expects a character vector
cat_WA <- category(res_WA, name = "WA")
tail(cat_WA)
#>    event_no event_name  peak_date   category  i_max duration p_moderate
#> 71       40   WA 2010c 2010-12-02  II Strong 2.6563       15         73
#> 72       31   WA 2008a 2008-04-14 III Severe 3.7693       34         62
#> 73       22    WA 1999 1999-05-22  II Strong 3.6017       95         59
#> 74       58    WA 2014 2014-08-31  II Strong 2.1952       12         58
#> 75       61    WA 2015 2015-10-02  II Strong 2.4257        7         57
#> 76       42   WA 2011b 2011-02-28 IV Extreme 6.5060       60         47
#>    p_strong p_severe p_extreme        season
#> 71       27        0         0 Spring/Summer
#> 72       29        3         0          Fall
#> 73       41        0         0   Fall/Winter
#> 74       42        0         0 Winter/Spring
#> 75       43        0         0        Spring
#> 76       32        8        13   Summer/Fall

# If the data were collected in the northern hemisphere
# we must let the function know this, as seen below
res_Med <- detect_event(ts2clm(sst_Med,
                        climatologyPeriod = c("1983-01-01", "2012-12-31")))
cat_Med <- category(res_Med, S = FALSE, name = "Med")
tail(cat_Med)
#>     event_no event_name  peak_date   category  i_max duration p_moderate
#> 114       62  Med 2012b 2012-08-20  II Strong 4.2207       18         56
#> 115      118  Med 2022b 2022-07-20  II Strong 4.7292       79         56
#> 116       70   Med 2014 2014-10-18  II Strong 3.3075      144         49
#> 117      119  Med 2022c 2022-11-03  II Strong 3.5117       74         46
#> 118       44  Med 2007b 2007-04-25 III Severe 4.0220       19         42
#> 119       88  Med 2018a 2018-04-28  II Strong 3.2901       10         20
#>     p_strong p_severe p_extreme      season
#> 114       44        0         0      Summer
#> 115       44        0         0 Summer/Fall
#> 116       50        0         0 Fall/Winter
#> 117       53        0         0 Fall/Winter
#> 118       53        5         0      Spring
#> 119       80        0         0      Spring

# One may also choose to have this function output the daily
# category classifications as well by setting: climatology = TRUE
cat_WA_daily <- category(res_WA, name = "WA", climatology = TRUE)
head(cat_WA_daily$climatology)
#>            t event_no intensity   category
#> 1 1984-06-03        1    1.5621 I Moderate
#> 2 1984-06-04        1    1.7868 I Moderate
#> 3 1984-06-05        1    1.9218 I Moderate
#> 4 1984-06-06        1    1.6672 I Moderate
#> 5 1984-06-07        1    1.5829 I Moderate
#> 6 1984-06-17        2    2.0198 I Moderate
```
