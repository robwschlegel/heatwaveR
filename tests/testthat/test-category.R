context("Test category.R")

# test_that("category() returns the correct tibbles and columns", {
#   ts <- heatwaveR::make_whole(sst_Med)
#   res <- heatwaveR::detect(data = ts, climatology_start = "1983-01-01", climatology_end = "2012-12-31")
#   cat_res <- heatwaveR::category(res)
#   expect_is(cat_res, "tbl_df")
#   expect_equal(ncol(cat_res), 11)
# })

## Test that column names (e.g. `x` and `y`) may be supplied as characters or objects
## Test that season splits work under broad circumstances
