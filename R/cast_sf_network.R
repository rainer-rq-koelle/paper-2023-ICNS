#' Cast a dataframe/tibble of LAT/LON positions to a unidirected sf-network
#'
#' @param .df dataframe/tibble of LAT/LON positions (of typically a trajectory/flight)
#' @param .crs coordinate reference system
#' @param .drop_coord
#'
#' @return sfnetwork object of trajectory
#' @export
#'
#' @examples
cast_latLon_to_sfnet <- function(.df, .crs = 4326, .drop_coord = FALSE){
  df <- .df %>%
    cast_latlon_to_pts(.crs = .crs, .drop_coord = .drop_coord) %>%
    sfnetworks::as_sfnetwork()
}
