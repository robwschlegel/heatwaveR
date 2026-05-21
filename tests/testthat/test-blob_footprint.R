context("blob_footprint")

skip_if_no_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) skip("sf not installed")
}

test_that("blob_footprint returns one polygon per voxel by default", {
  skip_if_no_sf()
  vox <- data.table::data.table(
    blob = 1L,
    lon = c(0, 0.1, 0.1),
    lat = c(0, 0, 0.1),
    date = as.Date("2020-01-01") + c(0, 0, 1),
    delta = c(1.5, 2.0, 1.8),
    area_km2 = 10
  )
  fp <- blob_footprint(vox, by = "day")
  expect_s3_class(fp, "sf")
  expect_equal(nrow(fp), 3L)
  expect_true(all(c("blob", "date", "delta", "area_km2") %in% names(fp)))
})

test_that("blob_footprint by = 'event' merges voxels into one geometry per blob", {
  skip_if_no_sf()
  vox <- data.table::data.table(
    blob = c(1L, 1L, 1L, 2L),
    lon  = c(0, 0.1, 0.1, 5),
    lat  = c(0, 0, 0.1, 5),
    date = as.Date("2020-01-01") + c(0, 0, 1, 0),
    delta = c(2, 2, 2, 1.6),
    area_km2 = 10
  )
  fp <- blob_footprint(vox, by = "event")
  expect_s3_class(fp, "sf")
  expect_equal(nrow(fp), 2L)
  expect_true("blob" %in% names(fp))
})

test_that("blob_footprint subsets by 'which'", {
  skip_if_no_sf()
  vox <- data.table::data.table(
    blob = c(1L, 1L, 2L),
    lon = c(0, 0.1, 5),
    lat = c(0, 0, 5),
    date = as.Date("2020-01-01"),
    delta = c(2, 2, 1.6),
    area_km2 = 10
  )
  fp <- blob_footprint(vox, which = 1L, by = "day")
  expect_true(all(fp$blob == 1L))
  expect_equal(nrow(fp), 2L)
})

test_that("blob_footprint handles empty selection", {
  skip_if_no_sf()
  vox <- data.table::data.table(
    blob = 1L, lon = 0, lat = 0,
    date = as.Date("2020-01-01"), delta = 2, area_km2 = 10
  )
  fp <- blob_footprint(vox, which = 99L)
  expect_equal(nrow(fp), 0L)
})

test_that("blob_footprint errors on missing required columns", {
  skip_if_no_sf()
  bad <- data.table::data.table(lon = 0, lat = 0)
  expect_error(blob_footprint(bad), "missing column")
})
