#' Title
#'
#' @param .trj 
#' @param target 
#'
#' @return
#' @export
#'
#' @examples
trj_add_target_bearing <- function(.trj_latlon_sf, .target_latlon_sf ){
  # define default NA for sf empty points when using lag/led
  empty <- sf::st_as_sfc("POINT(EMPTY)", crs = 4326)

  final <- .trj_latlon_sf |> 
 
  # now calculate the data we need  
    mutate(
        THR      = sf::st_geometry(.target_latlon_sf)
      , DIST_THR = sf::st_distance(geometry, THR, by_element = TRUE)
      , distance_to_next = sf::st_distance(
                geometry
              , dplyr::lead(geometry, default = empty)
              , by_element = TRUE)
      , BRG = geosphere::bearingRhumb(
                cbind(LON, LAT)
              , cbind(.target_latlon_sf$LON, .target_latlon_sf$LAT)
              )
      )
  return(final)
}