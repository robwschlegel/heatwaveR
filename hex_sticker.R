# hex_sticker.R
# The purpose of this script is to create hex stickers!
# Robert Schlegel
# May 3rd, 2018

# Load libraries ----------------------------------------------------------

library(hexSticker)
library(heatwaveR)
library(tidyverse)

# Create logo -------------------------------------------------------------

# The event line figure
ts_dat <- ts2clm(sst_WA, climatologyPeriod = c("1983-01-01", "2012-12-31"))
res <- detect_event(ts_dat)

el <- event_line(res, spread = 200, metric = "intensity_cumulative",
                 start_date = "2010-10-01", end_date = "2011-08-30")
el

# The geom_flame figure
mhw <- res$clim
mhw <- mhw[10627:10690,]
gf <- ggplot(mhw, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh)) +
  theme_void()
gf

gf2 <- ggplot(mhw, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh), fill = "red") +
  theme_void()
gf2

# The category flame
clim_cat <- mhw %>%
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff)

# Set category fill colours
fillColCat <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

gf3 <- ggplot(data = clim_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  scale_fill_manual(name = NULL, values = fillColCat, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y") +
  theme_void()
gf3

# I prefer the geom_flame

# Place it on a sticker ---------------------------------------------------

sticker(gf3, package = "heatwaveR", p_size = 22, s_x = 1.06, s_y = 1.1, s_width = 1.6, s_height = 1.6,
        h_fill = "royalblue4", h_color = "#9e0000", p_family = "OperatorMono-MediumItalic", filename = "logo.png")

# sticker(gf2, package = "heatwaveR", p_size = 8, s_x = 1.05, s_y = 0.8, s_width = 1.4, s_height = 1.0,
#         h_fill = "navy", h_color = "white", p_family = "UbuntuMono-BI", filename = "logo.png")
# devtools::check() creates notes when there are things in the root that it doesn't expect
# This includes any .png files that are not labeled "logo.png"
