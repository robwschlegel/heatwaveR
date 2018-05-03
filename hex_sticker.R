# hex_sticker.R
# The purpose of this script is to create hex stickers!
# Robert Schlegel
# May 3rd, 2018

# Load libraries ----------------------------------------------------------

library(hexSticker)


# Create logo -------------------------------------------------------------

# The event line figure
ts_dat <- make_whole(sst_WA)
res <- detect(ts_dat, climatology_start = "1983-01-01",
               climatology_end = "2012-12-31")

el <- event_line(res, spread = 200, metric = "int_cum", start_date = "2010-10-01", end_date = "2011-08-30")
el

# The geom_flame figure
ts_dat <- make_whole(sst_WA)
res <- detect(ts_dat, climatology_start = "1983-01-01",
               climatology_end = "2012-12-31")
mhw <- res$clim
mhw <- mhw[10627:10690,]
gf <- ggplot(mhw, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh_clim_year)) +
  theme_void()
gf

# I prefer the geom_flame

# Place it on a sticker ---------------------------------------------------

sticker(gf, package = "heatwaveR", p_size = 22, s_x = 1.07, s_y = 0.8, s_width = 1.4, s_height = 1.0,
        h_fill = "steelblue3", h_color = "red", p_family = "OperatorMono-MediumItalic", filename = "logo.png")

