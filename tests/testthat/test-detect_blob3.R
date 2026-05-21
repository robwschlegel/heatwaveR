context("detect_blob3")

# ---- helpers -----------------------------------------------------------------

# Build a long data.table for a synthetic gridded event mask.
# event_array: logical array of shape c(nx, ny, nt). delta_array: numeric array
# of shape c(nx, ny, nt) giving Delta = temp - seas at each voxel. lons, lats,
# dates: coordinate vectors of matching length.
make_long <- function(event_array, delta_array, lons, lats, dates) {
  nx <- length(lons); ny <- length(lats); nt <- length(dates)
  stopifnot(identical(dim(event_array), c(nx, ny, nt)))
  stopifnot(identical(dim(delta_array), c(nx, ny, nt)))
  # Column-major order: i (lon) fastest, j (lat) middle, k (time) slowest.
  data.table::data.table(
    lon   = rep(lons, times = ny * nt),
    lat   = rep(rep(lats, each = nx), times = nt),
    t     = rep(dates, each = nx * ny),
    temp  = as.numeric(as.vector(delta_array)),
    seas  = 0,
    event = as.logical(as.vector(event_array))
  )
}

# ---- labelling correctness ---------------------------------------------------

test_that("3-D labelling: diagonal-only contact stays separate (6-connectivity)", {
  m <- array(0L, dim = c(3, 3, 1))
  m[1, 1, 1] <- 1L
  m[2, 2, 1] <- 1L
  m[3, 3, 1] <- 1L
  lab <- label_components_3d_cpp(as.integer(m), 3L, 3L, 1L, FALSE)
  expect_equal(attr(lab, "n_components"), 3L)
})

test_that("3-D labelling: union-find resolves U-shape into one component", {
  m <- array(0L, dim = c(5, 3, 1))
  m[1, 1:3, 1] <- 1L
  m[5, 1:3, 1] <- 1L
  m[2:4, 3, 1] <- 1L
  lab <- label_components_3d_cpp(as.integer(m), 5L, 3L, 1L, FALSE)
  expect_equal(attr(lab, "n_components"), 1L)
})

test_that("3-D labelling: time connectivity links cells across consecutive days", {
  m <- array(0L, dim = c(2, 2, 3))
  m[1, 1, ] <- 1L  # column connected through all 3 days
  lab <- label_components_3d_cpp(as.integer(m), 2L, 2L, 3L, FALSE)
  expect_equal(attr(lab, "n_components"), 1L)
})

test_that("3-D labelling: dateline wrap merges separated edge components", {
  m <- array(0L, dim = c(5, 3, 1))
  m[1, 2, 1] <- 1L
  m[5, 2, 1] <- 1L
  lab_off <- label_components_3d_cpp(as.integer(m), 5L, 3L, 1L, FALSE)
  lab_on  <- label_components_3d_cpp(as.integer(m), 5L, 3L, 1L, TRUE)
  expect_equal(attr(lab_off, "n_components"), 2L)
  expect_equal(attr(lab_on,  "n_components"), 1L)
})

test_that("3-D labelling: empty mask returns zero components", {
  m <- array(0L, dim = c(3, 3, 3))
  lab <- label_components_3d_cpp(as.integer(m), 3L, 3L, 3L, FALSE)
  expect_equal(attr(lab, "n_components"), 0L)
})

test_that("3-D labelling: all-true mask returns one component", {
  m <- array(1L, dim = c(3, 3, 3))
  lab <- label_components_3d_cpp(as.integer(m), 3L, 3L, 3L, FALSE)
  expect_equal(attr(lab, "n_components"), 1L)
})

# ---- detect_blob3() end-to-end -----------------------------------------------

test_that("detect_blob3: planted MHW recovered with correct rank and metrics", {
  set.seed(42)
  lons  <- seq(15.0, 16.0, by = 0.05)
  lats  <- seq(-32.0, -31.0, by = 0.05)
  dates <- as.Date("2020-01-01") + 0:9
  nx <- length(lons); ny <- length(lats); nt <- length(dates)

  delta <- array(stats::rnorm(nx * ny * nt, 0, 0.3), dim = c(nx, ny, nt))
  delta[5:9, 5:9, 3:7] <- 3.0      # big planted MHW blob
  delta[19, 18, 5]      <- 4.0      # high-intensity singleton
  event <- delta > 1.5
  long <- make_long(event, delta, lons, lats, dates)

  res <- detect_blob3(long, return = c("event", "daily", "voxel"))

  # The big blob has 5x5x5 = 125 voxels and ranks first by |cumI|.
  expect_gte(nrow(res$event), 1L)
  top <- res$event[rank == 1]
  expect_equal(top$n_voxels, 125L)
  expect_equal(top$duration, 5L)
  expect_equal(top$date_start, as.Date("2020-01-03"))
  expect_equal(top$date_end,   as.Date("2020-01-07"))
  expect_equal(round(top$mean_delta, 6), 3)
  expect_gt(top$peakArea_km2, 0)
  # Voxel table reconstructs the footprint exactly.
  expect_equal(nrow(res$voxel[blob == top$blob]), 125L)
})

test_that("detect_blob3: minVoxels filter and topN both apply", {
  set.seed(7)
  lons  <- seq(15.0, 16.0, by = 0.05)
  lats  <- seq(-32.0, -31.0, by = 0.05)
  dates <- as.Date("2020-01-01") + 0:9
  nx <- length(lons); ny <- length(lats); nt <- length(dates)

  delta <- array(0, dim = c(nx, ny, nt))
  delta[3:4, 3:4, 1:2] <- 2  # 16-voxel blob
  delta[10, 10, 5]      <- 3 # singleton
  event <- delta > 0.5
  long <- make_long(event, delta, lons, lats, dates)

  res5  <- detect_blob3(long, minVoxels = 5L)
  res1  <- detect_blob3(long, minVoxels = 1L)
  expect_lt(nrow(res5$event), nrow(res1$event))

  res_top1 <- detect_blob3(long, topN = 1L)
  expect_equal(nrow(res_top1$event), 1L)
  expect_equal(res_top1$event$rank, 1L)
})

test_that("detect_blob3: cold-spell input gets negative cumI and ranks by magnitude", {
  set.seed(11)
  lons  <- seq(0, 1, by = 0.05)
  lats  <- seq(-5, -4, by = 0.05)
  dates <- as.Date("2020-01-01") + 0:9
  nx <- length(lons); ny <- length(lats); nt <- length(dates)

  delta <- array(0, dim = c(nx, ny, nt))
  delta[5:9, 5:9, 3:7] <- -3.0   # cold-spell blob
  delta[15, 15, 5]      <- -4.0   # cold singleton
  event <- delta < -1.5
  long <- make_long(event, delta, lons, lats, dates)

  res <- detect_blob3(long, return = c("event", "daily"))

  expect_true(all(res$event$cumI_km2_day < 0))
  expect_true(all(res$event$mean_delta < 0))
  expect_equal(attr(res$event, "polarity"), "cool")
  # Ranking is by |cumI_km2_day| so the larger-volume blob ranks first.
  expect_equal(res$event$rank[order(-abs(res$event$cumI_km2_day))], res$event$rank)
})

test_that("detect_blob3: empty event mask returns empty tables", {
  lons  <- seq(0, 1, by = 0.1); lats <- seq(0, 1, by = 0.1)
  dates <- as.Date("2020-01-01") + 0:4
  long <- data.table::CJ(lon = lons, lat = lats, t = dates)
  long[, `:=`(temp = 0, seas = 0, event = FALSE)]
  res <- detect_blob3(long)
  expect_equal(nrow(res$event), 0L)
  expect_equal(nrow(res$daily), 0L)
})

test_that("detect_blob3: irregular grid throws", {
  lons  <- c(0, 0.1, 0.25, 0.4)  # irregular spacing
  lats  <- seq(0, 1, by = 0.1)
  dates <- as.Date("2020-01-01") + 0:2
  long <- data.table::CJ(lon = lons, lat = lats, t = dates)
  long[, `:=`(temp = 0, seas = 0, event = FALSE)]
  expect_error(detect_blob3(long), "Longitude grid is not regular")
})

test_that("detect_blob3: missing required column throws", {
  lons <- seq(0, 1, by = 0.1); lats <- seq(0, 1, by = 0.1)
  dates <- as.Date("2020-01-01") + 0:2
  long <- data.table::CJ(lon = lons, lat = lats, t = dates)
  long[, `:=`(temp = 0, seas = 0)]
  expect_error(detect_blob3(long), "Missing column")
})

# ---- round-trip with detect_event3() -----------------------------------------

test_that("detect_blob3: event-voxel count is conserved across spatial events", {
  # Construct a per-pixel event table directly and check that the spatial
  # function preserves the total event-voxel count.
  set.seed(2025)
  lons <- seq(0, 0.4, by = 0.1); lats <- seq(0, 0.4, by = 0.1)
  dates <- as.Date("2020-01-01") + 0:19
  nx <- length(lons); ny <- length(lats); nt <- length(dates)

  delta <- array(stats::rnorm(nx * ny * nt, 0, 0.4), dim = c(nx, ny, nt))
  delta[2:4, 2:4, 5:10]  <- delta[2:4, 2:4, 5:10] + 3
  delta[1, 5, 15]        <- 2.5  # disjoint singleton
  event <- delta > 1.5
  long <- make_long(event, delta, lons, lats, dates)
  total_evt <- sum(long$event)
  if (total_evt == 0L) skip("planted warming did not exceed threshold")

  res <- detect_blob3(long, return = c("event", "daily"))
  expect_equal(sum(res$event$n_voxels), total_evt)
})
