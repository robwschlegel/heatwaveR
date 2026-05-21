#' Detect spatial heatwave or cold-spell blobs.
#'
#' Takes a long \code{data.table} of per-pixel event detections (the output of
#' \code{\link{detect_event3}} applied across a regular longitude-latitude grid
#' and bound across grid cells) and identifies contiguous regions of event-affected
#' voxels in space and time. Returns ranked spatial events with integrated
#' metrics. The function operates on a boolean event flag regardless of polarity,
#' so cold-spell input (from \code{detect_event3()} with \code{pctile = 10}) is
#' handled with the same code path: signed metrics retain sign, and ranking by
#' signed metrics uses magnitude.
#'
#' @param data A \code{data.table} (or \code{data.frame} coerced to one) with one
#'   row per (lon, lat, date). Must contain the columns named by \code{x},
#'   \code{y}, \code{t}, \code{temp}, \code{seas}, and \code{event}. The grid
#'   spacing in \code{x} and \code{y} must be regular. Time spacing must be
#'   regular and daily.
#' @param x,y,t Column names for the longitude, latitude, and date axes.
#' @param temp Column name for the measured value (used together with
#'   \code{seas} to derive the anomaly \eqn{\Delta = temp - seas}).
#' @param seas Column name for the seasonal climatology.
#' @param event Column name for the per-pixel boolean event flag.
#' @param connectivity Voxel adjacency. Only \code{6} (face-adjacent) is
#'   supported in v1.
#' @param wrapDateline If \code{TRUE}, the first and last longitude columns are
#'   treated as adjacent during labelling. Default \code{FALSE}.
#' @param minVoxels Discard blobs with fewer than this many voxels. Default 1.
#' @param topN If non-NULL, retain only the largest \code{topN} blobs after
#'   ranking by \code{rankBy}.
#' @param rankBy Metric used for ranking. One of \code{"cumI_km2_day"} (default,
#'   ranked by magnitude so the same default works for heatwaves and cold spells),
#'   \code{"peakArea_km2"}, \code{"volume_km2_day"}, or \code{"duration"}.
#' @param cellAreaMethod \code{"coslat"} (default) for regular lon-lat grids
#'   with cos(latitude) weighting; \code{"constant"} for unit-weighted cells
#'   (testing only).
#' @param return Character vector. Subset of \code{c("event", "daily", "voxel")}.
#'   The \code{voxel} table reconstructs the full footprint and is heavy; opt-in.
#'
#' @return A named list with the requested tables.
#'   \itemize{
#'     \item \code{event}: one row per spatial event with summary metrics and
#'       a \code{rank} column.
#'     \item \code{daily}: one row per (event, date) with area, mean and max
#'       anomaly, centroid, and bounding box.
#'     \item \code{voxel}: one row per (event, lon, lat, date) with the per-cell
#'       anomaly and area. Heavy; opt-in.
#'   }
#'   The \code{event} table carries attributes \code{polarity} ("warm" or
#'   "cool"), \code{filters} (the call's filter arguments), and
#'   \code{n_components_raw} (component count before \code{minVoxels} filtering).
#'
#' @examples
#' \dontrun{
#' # Per-pixel events on a gridded dataset.
#' grid_events <- detect_event3(climatology_long)$climatology
#' res <- detect_blob3(grid_events, topN = 10)
#' res$event   # top 10 spatial blobs by |cumI|
#' res$daily   # daily area + centroid trajectory
#' }
#'
#' @export
detect_blob3 <- function(data,
                         x = "lon", y = "lat", t = "t",
                         temp = "temp", seas = "seas", event = "event",
                         connectivity = 6L,
                         wrapDateline = FALSE,
                         minVoxels = 1L,
                         topN = NULL,
                         rankBy = c("cumI_km2_day", "peakArea_km2",
                                    "volume_km2_day", "duration"),
                         cellAreaMethod = c("coslat", "constant"),
                         return = c("event", "daily")) {

  rankBy <- match.arg(rankBy)
  cellAreaMethod <- match.arg(cellAreaMethod)
  if (!identical(as.integer(connectivity), 6L)) {
    stop("Only connectivity = 6 is supported in v1.")
  }
  return <- match.arg(return, several.ok = TRUE,
                      choices = c("event", "daily", "voxel"))

  if (!data.table::is.data.table(data)) data <- data.table::as.data.table(data)

  needed <- c(x, y, t, temp, seas, event)
  miss <- setdiff(needed, names(data))
  if (length(miss)) stop("Missing column(s): ", paste(miss, collapse = ", "))

  lons <- sort(unique(data[[x]]))
  lats <- sort(unique(data[[y]]))
  dates <- sort(unique(data[[t]]))
  nx <- length(lons); ny <- length(lats); nt <- length(dates)

  if (nx < 2L || ny < 2L || nt < 1L) {
    stop("Grid is too small: nx=", nx, " ny=", ny, " nt=", nt)
  }

  dlon <- diff(lons)
  dlat <- diff(lats)
  tol_rel <- 1e-3
  if (max(dlon) - min(dlon) > tol_rel * mean(dlon)) {
    stop("Longitude grid is not regular (max relative deviation = ",
         signif((max(dlon) - min(dlon)) / mean(dlon), 3), ").")
  }
  if (max(dlat) - min(dlat) > tol_rel * mean(dlat)) {
    stop("Latitude grid is not regular (max relative deviation = ",
         signif((max(dlat) - min(dlat)) / mean(dlat), 3), ").")
  }

  # Restrict to event voxels first; extract coordinates and anomaly in lockstep.
  keep <- as.logical(data[[event]]) & !is.na(data[[event]])
  if (!any(keep)) {
    empty <- data.table::data.table()
    return(list(event = empty, daily = empty))
  }

  ix_evt <- match(data[[x]][keep], lons)
  iy_evt <- match(data[[y]][keep], lats)
  it_evt <- match(data[[t]][keep], dates)
  delta_evt <- data[[temp]][keep] - data[[seas]][keep]

  # Flat index into the 3-D mask. R is 1-based; we pass to C++ as a flat
  # integer vector of length nx*ny*nt with 1 at event positions, 0 elsewhere.
  flat_idx_evt <- ix_evt + nx * (iy_evt - 1L) + nx * ny * (it_evt - 1L)
  mask <- integer(as.numeric(nx) * ny * nt)
  mask[flat_idx_evt] <- 1L

  labels <- label_components_3d_cpp(mask, nx, ny, nt, wrapDateline)
  n_comp <- attr(labels, "n_components")
  if (n_comp == 0L) {
    empty <- data.table::data.table()
    return(list(event = empty, daily = empty))
  }

  lab_evt <- labels[flat_idx_evt]

  # Cell area per latitude row.
  R_earth_km <- 6371.0
  dlon_rad <- mean(dlon) * pi / 180
  dlat_rad <- mean(dlat) * pi / 180
  cell_area_lat <- if (cellAreaMethod == "coslat") {
    R_earth_km * R_earth_km * dlon_rad * dlat_rad * cos(lats * pi / 180)
  } else {
    rep(1.0, ny)
  }

  # C++ reducer: returns the per-(blob, day) summary.
  dl <- blob_daily_summary_cpp(lab_evt, ix_evt, iy_evt, it_evt,
                               delta_evt, cell_area_lat, lons, lats, nt)
  daily <- data.table::as.data.table(dl)
  daily[, date := dates[t_idx]]
  data.table::setcolorder(daily,
    c("blob", "date", "area_km2", "mean_delta", "max_delta",
      "centroid_lon", "centroid_lat",
      "lon_min", "lon_max", "lat_min", "lat_max", "n_cells", "t_idx"))

  # Per-event aggregation from the daily table.
  events <- daily[, .(
    date_start     = min(date),
    date_end       = max(date),
    duration       = as.integer(max(t_idx) - min(t_idx) + 1L),
    n_voxels       = sum(n_cells),
    volume_km2_day = sum(area_km2),
    cumI_km2_day   = sum(mean_delta * area_km2),
    mean_delta     = stats::weighted.mean(mean_delta, area_km2),
    max_delta      = max(max_delta),
    bbox_lon_min   = min(lon_min),
    bbox_lon_max   = max(lon_max),
    bbox_lat_min   = min(lat_min),
    bbox_lat_max   = max(lat_max),
    peakArea_km2       = max(area_km2),
    date_peak          = date[which.max(area_km2)],
    centroid_lon_peak  = centroid_lon[which.max(area_km2)],
    centroid_lat_peak  = centroid_lat[which.max(area_km2)]
  ), by = blob]

  # Filter.
  events <- events[n_voxels >= minVoxels]
  daily  <- daily[blob %in% events$blob]

  # Rank.
  signed_metrics <- c("cumI_km2_day")
  if (rankBy %in% signed_metrics) {
    ord <- order(-abs(events[[rankBy]]))
  } else {
    ord <- order(-events[[rankBy]])
  }
  events <- events[ord]
  events[, rank := seq_len(.N)]

  # topN.
  if (!is.null(topN)) {
    keep_blobs <- events$blob[seq_len(min(topN, nrow(events)))]
    events <- events[blob %in% keep_blobs]
    daily  <- daily[blob %in% keep_blobs]
  }

  # Polarity flag + sign-mixed warning.
  med_delta <- stats::median(daily$mean_delta, na.rm = TRUE)
  if (nrow(events) > 1L &&
      any(events$mean_delta > 0) && any(events$mean_delta < 0)) {
    warning("Mixed-sign anomalies across blobs: input may combine MHW and MCS rows.")
  }
  attr(events, "polarity") <- if (isTRUE(med_delta > 0)) "warm" else "cool"
  attr(events, "filters") <- list(minVoxels = minVoxels, topN = topN,
                                  rankBy = rankBy, connectivity = connectivity,
                                  wrapDateline = wrapDateline)
  attr(events, "n_components_raw") <- n_comp

  out <- list()
  if ("event" %in% return) out$event <- events
  if ("daily" %in% return) out$daily <- daily

  if ("voxel" %in% return) {
    in_keep <- lab_evt %in% events$blob
    out$voxel <- data.table::data.table(
      blob = lab_evt[in_keep],
      lon  = lons[ix_evt[in_keep]],
      lat  = lats[iy_evt[in_keep]],
      date = dates[it_evt[in_keep]],
      delta = delta_evt[in_keep],
      area_km2 = cell_area_lat[iy_evt[in_keep]]
    )
    data.table::setkey(out$voxel, blob, date, lon, lat)
  }

  out
}

## Silence R CMD check NOTEs from data.table non-standard evaluation.
utils::globalVariables(c(
  ".",
  "t_idx", "n_cells", "area_km2", "mean_delta", "max_delta",
  "lon_min", "lon_max", "lat_min", "lat_max",
  "centroid_lon", "centroid_lat",
  "blob", "n_voxels", "lon", "lat", "date", "delta", "rank"
))
