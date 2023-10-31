# package_development.R
# Tuesday, April 24th, 2018
# The purpose of this script is to have written down the steps necessary
# to create, test and maintain the heatwaveR package


# Getting started ---------------------------------------------------------

# These packages are centrally important
library(usethis)
library(devtools)
library(testthat)
library(pkgdown)

# Disable package build note about not finding local time
Sys.setenv('_R_CHECK_SYSTEM_CLOCK_' = 0)

# edit_r_profile()
# create_package("heatwaveR")


# Next steps --------------------------------------------------------------

# After running the above line your new package skeleton will be created
# Up next one needs to edit the DESCRIPTION file

# Then run this to create license:
# options(usethis.full_name = "Robert William Schlegel")
# use_mit_license()

# Then run this to create a function skeleton:
# use_r(name = "make_whole")
# use_r(name = "detect")
# use_r(name = "exceedance")
# use_r(name = "block_average")
# use_r(name = "geoms")
# use_r(name = "graph_functions")
# use_r(name = "category")
# use_r(name = "detect_clim")
# use_r(name = "detect_event")
# use_r(name =  "smooth_percentile")
# use_r(name =  "clim_spread")
# use_r(name =  "clim_calc")
# use_r(name =  "proto_events")
# use_r(name =  "proto_gaps")

# One must still then go about writing the function

# Once you've made a bit, run this to check the package:
# load_all()

# To insert Roxygen skeleton for function go: code -> insert Roxygen skeleton

# After finishing the roxygen skeleton run:
# document()
# This creates the documentation

# Then click "Install and Restart" in the build tab
# This will also load your new package

# Once built, set up your Git credentials:
# use_git_config(user.name = "Robert William Schlegel", user.email = "robwschlegel@gmail.com")

# Then run this to create the other bits Git will need:
# use_git()

# Finally, to push it to your Github as a new repo first start a new repo on
# your GitHub account with the exact same name.
# Do not add a README file or anything else. Leave it completely bare
# Then scroll down on the newly created landing page and click on the
# copy button for how to connect a remote repo.
# This will give you two lines of command line code
# Paste them into a terminal that is pointed at the home directory for the package
# This will then connect and auto push everything to your GitHub account

# Then run this to create the overall documentation for your package:
# use_package_doc()
# And again run this to update it all:
# document()

# Use the following line to add a package to the import:
# use_package(package = "ggplot2")
# use_package(package = "plyr")
# use_package(package = "tidyr")
# This makes sure your external dependencies are in order
# Or just add it manually to DESCRIPTION

# Add the following line to function documentation to ensure use of a needed function:
#' @importFrom magrittr "%>%"
# But only do this after you've already added the skeleton as detailed above


# Testing -----------------------------------------------------------------

# To begin testing first run:
# use_testthat()

# Then to test a function:
# use_test("make_whole")
# use_test("detect")
# use_test("exceedance")
# use_test("block_average")
# use_test("graph_functions")
# use_test("geoms")
# use_test("category")
# use_test("clim_spread")
# use_test("clim_calc")
# use_test("smooth_percentile")
# use_test("ts2clm")
# use_test("proto_event")
# use_test("detect_event")
# use_test("na_interp")

# After creating this documentation one will likely need to edit it
# Here is an example:
# context("make_whole")

# test_that("make_whole() returns a dataframe", {
#   expect_is(make_whole(), "data.frame")
# })

# Always write tests immediately as you almost certainly won't do it later

# Give it all a thorough look over with:
# devtools::check()
# One must have zero errors, warnings, notes


# Importing data ----------------------------------------------------------

# First run:
# use_data_raw()

# Then go and throw the data in the "data-raw" folder that was created

# Then run this on the data in question after loading it:
# load("data-raw/sst_Med.RData")
# use_data(sst_Med)
# load("data-raw/sst_NW_Atl.RData")
# use_data(sst_NW_Atl)
# load("data-raw/sst_WA.RData")
# use_data(sst_WA)

# After doing so one must manually create an Rscript for these data
# There is currently no shortcut for this
# use_r(name = "sst_Med")
# use_r(name = "sst_NW_Atl")
# use_r(name = "sst_WA")
# Once this has been done run:
# document()


# Additional development --------------------------------------------------

# First add a read me file with:
# use_readme_md()

# Run this in console with the project open to set up GitHub actions
# usethis::use_github_action_check_standard()

# Pop in your build status from GitHub actions by copying the output from the above line
# [![R-CMD-check](https://github.com/robwschlegel/heatwaveR/workflows/R-CMD-check/badge.svg)](https://github.com/robwschlegel/heatwaveR/actions)

# Add a coverage status badge
# use_coverage(type = "codecov")
# Looks like:
# [![Coverage status](https://codecov.io/gh/robwschlegel/heatwaveR/branch/master/graph/badge.svg)](https://codecov.io/github/robwschlegel/heatwaveR?branch=master)

# Then add a code of conduct:
# use_code_of_conduct()


# Vignettes ---------------------------------------------------------------

# Now to create some better documentation
# use_vignette(name = "gridded_event_detection")
# use_vignette(name = "complex_clims")


# pkgdown -----------------------------------------------------------------

# Went over this only briefly
# Good to do, but a bit more finicky

# Start with this to build skeleton:
# use_pkgdown()

# Then run:
# pkgdown::build_site()

# After that, spruce things up a bit:
# sink("_pkgdown.yml")
# template_navbar()
# template_reference()
# template_articles()
# sink()

# Don't forget that after you make any changes you must run this then push again:
build_site()
# build_news()
# build_article("complex_clims")


# News --------------------------------------------------------------------

# Add a news tracking file with
# use_news_md()

# The following options are relatively garbage
# Decided not to use them
# Better off manually managing the NEWS and DESCRIPTION files
# # When you want to assign a version number to the current state of your R package, call
# fledge::bump_version("patch")
# # OR
# fledge::bump_version("minor")
# # OR
# fledge::bump_version("major")
#
# fledge::finalize_version()
#
# fledge::tag_version(force = TRUE)


# Package logs ------------------------------------------------------------

# An example of how to check package downloads
ggplot2_logs <- cranlogs::cran_downloads(packages = c("ggplot2", "dplyr"),
                                         when = "last-month")
ggplot2::ggplot(ggplot2_logs) +
  ggplot2::geom_line(aes(date, count, col = package)) +
  viridis::scale_color_viridis(discrete = TRUE)

# Many other options for package analytics


# Submitting to CRAN ------------------------------------------------------

# Some good additional advice may be found here:
# http://kbroman.org/pkg_primer/pages/cran.html

# Run check and make sure there are no ERROR, WARNING, or NOTE

# After that run the following command to check the package on a
# windows OS if you are not currently running on that
devtools::check_win_release()

# Or check specific CRAN flavours via the rhub package
# https://blog.r-hub.io/2019/04/25/r-devel-linux-x86-64-debian-clang/
# platforms_df <- as.data.frame(rhub::platforms())

# Local check - Requires bash and Docker
# rhub::local_check_linux(image = "rhub/debian-gcc-devel")

# Online check
rhub::check(platform = "debian-gcc-devel")

# Then use the gear button in the top right pane to 'Build Source Package'

# The source package should be submitted via this web form:
# http://xmpalantir.wu.ac.at/cransubmit/


# Publishing version ------------------------------------------------------

# Once the new version is up on CRAN it is time to re-publish this in a couple of places

# Up first is the Journal of open-source software (JOSS)
# http://joss.theoj.org/papers/9353247bebdfcc0f7357a759381416bb
# Actually, it looks like this doesn't need to be updated with new releases as it is
# pointed at the following source, which does need updating...

# Second is to publish the new version on GitHub
# Instructions for how to do so are here:
# https://help.github.com/articles/creating-releases/

# This then should automagically update the version listed on zenodo
# https://zenodo.org/record/1324309
# If not then follow these instructions:
# http://help.zenodo.org/#versioning
# Or just go to the Zenodo website for heatwaveR and click the big green button 'New version'


# Speed tests -------------------------------------------------------------

library(profvis)
library(heatwaveR)
profvis(detect_event(ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))))
profvis(detect_event(ts2clm3(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31"))))
