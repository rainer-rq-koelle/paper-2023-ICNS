#' Calculate distance between successive points in a dataframe of LAT/LON positions
#'
#' @param .trj
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
add_flight_distance <- function(.trj, unit = "NM"){
  df <- .trj
  
  #-------------- units
  # distHaversine defaults to radius of Earth in meters
  r_NM = 3443.92
  unit_radius = r_NM
  #----------------------------------------------------
  
  df <- df %>%
    dplyr::mutate(
      LAT_PREV = lag(LAT, default = first(LAT))
      ,LON_PREV = lag(LON, default = first(LON))
      ,DIST_FLOWN = geosphere::distHaversine( cbind(LON,LAT)
                                              ,cbind(LON_PREV, LAT_PREV)
                                              , r = unit_radius)
      ,DIST_FLOWN = cumsum(DIST_FLOWN)
    ) %>%
    # remove additional columns
    select(-LAT_PREV, - LON_PREV)
  return(df)
}


# =================== sf based solution ==========================
# library(sf)
# library(dplyr)
#
# df %>%
#   group_by(gr) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T),
#   )
