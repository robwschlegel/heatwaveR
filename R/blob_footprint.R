#' Convert a spatial blob's footprint to an sf object.
#'
#' Takes the \code{voxel} table from \code{\link{detect_blob3}} and produces
#' an \code{sf} object describing the footprint of one or more blobs. Each
#' grid cell is represented as a square polygon centred on its (lon, lat)
#' coordinate, with side length matching the grid spacing inferred from the
#' input.
#'
#' This function is the only entry point in \code{heatwaveR} that touches the
#' optional \code{sf} dependency. \code{sf} must be installed to use it.
#'
#' @param voxel A \code{data.table} of voxels from
#'   \code{detect_blob3(..., return = c(..., "voxel"))} with columns
#'   \code{blob}, \code{lon}, \code{lat}, \code{date}, \code{delta},
#'   \code{area_km2}.
#' @param which Blob identifier(s) to extract. \code{NULL} (default) keeps all
#'   blobs in \code{voxel}.
#' @param by One of \code{"day"} (default) or \code{"event"}. With \code{"day"}
#'   the result has one row per (blob, date) cell, suitable for animation or
#'   per-day mapping. With \code{"event"} the per-day cells are unioned into
#'   one multi-polygon per blob.
#' @param cell_size If \code{NULL}, the cell side length is inferred from the
#'   modal lon and lat spacing in \code{voxel}. Otherwise, a length-2 numeric
#'   in degrees giving (dlon, dlat).
#' @param crs Coordinate reference system passed to \code{sf::st_sf()}. Default
#'   \code{4326} (WGS 84).
#'
#' @return An \code{sf} object. With \code{by = "day"}, columns are \code{blob},
#'   \code{date}, \code{delta}, \code{area_km2}, and the geometry. With
#'   \code{by = "event"}, columns are \code{blob} and the geometry only.
#'
#' @examples
#' \dontrun{
#'   res <- detect_blob3(grid_events, topN = 5, return = c("event", "voxel"))
#'   foot <- blob_footprint(res$voxel, which = res$event$blob[1], by = "event")
#'   plot(foot["blob"])
#' }
#'
#' @export
blob_footprint <- function(voxel,
                           which = NULL,
                           by = c("day", "event"),
                           cell_size = NULL,
                           crs = 4326) {
  by <- match.arg(by)
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for blob_footprint(). Install it with ",
         "install.packages('sf').")
  }
  required <- c("blob", "lon", "lat", "date", "delta", "area_km2")
  miss <- setdiff(required, names(voxel))
  if (length(miss)) stop("voxel table is missing column(s): ",
                         paste(miss, collapse = ", "))

  # Infer cell size from the full voxel table before subsetting; that way a
  # one-cell selection still gets a sensible spacing.
  if (is.null(cell_size)) {
    lon_u <- sort(unique(voxel$lon))
    lat_u <- sort(unique(voxel$lat))
    if (length(lon_u) >= 2L && length(lat_u) >= 2L) {
      dlon <- min(diff(lon_u)); dlat <- min(diff(lat_u))
    } else {
      dlon <- NA_real_; dlat <- NA_real_
    }
  } else {
    if (length(cell_size) != 2L) stop("cell_size must be length 2.")
    dlon <- cell_size[1]; dlat <- cell_size[2]
  }

  if (!is.null(which)) {
    voxel <- voxel[voxel$blob %in% which, ]
  }
  if (nrow(voxel) == 0L) {
    return(sf::st_sf(blob = integer(0),
                     geometry = sf::st_sfc(crs = crs)))
  }
  if (is.na(dlon) || is.na(dlat)) {
    stop("Cell size cannot be inferred from fewer than two unique ",
         "lon/lat values. Pass cell_size = c(dlon, dlat).")
  }

  hx <- dlon / 2; hy <- dlat / 2
  # Build one polygon per voxel.
  geoms <- lapply(seq_len(nrow(voxel)), function(i) {
    x <- voxel$lon[i]; y <- voxel$lat[i]
    sf::st_polygon(list(rbind(
      c(x - hx, y - hy), c(x + hx, y - hy),
      c(x + hx, y + hy), c(x - hx, y + hy),
      c(x - hx, y - hy)
    )))
  })
  sf_obj <- sf::st_sf(
    blob     = voxel$blob,
    date     = voxel$date,
    delta    = voxel$delta,
    area_km2 = voxel$area_km2,
    geometry = sf::st_sfc(geoms, crs = crs)
  )

  if (by == "event") {
    parts <- split(sf_obj, sf_obj$blob)
    merged <- lapply(parts, function(g) {
      sf::st_sf(blob = g$blob[1L],
                geometry = sf::st_union(sf::st_geometry(g)),
                crs = crs)
    })
    sf_obj <- do.call(rbind, merged)
    rownames(sf_obj) <- NULL
  }
  sf_obj
}
