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
      LAT_PREV =  dplyr::lag(LAT, default = dplyr::first(LAT))
      ,LON_PREV = dplyr::lag(LON, default = dplyr::first(LON))
      ,DIST_NEXT = geosphere::distHaversine(  cbind(LON,LAT)
                                              ,cbind(LON_PREV, LAT_PREV)
                                              , r = unit_radius)
      ,DIST_FLOWN = cumsum(DIST_NEXT)
    ) %>%
    # remove additional columns
    dplyr::select(-LAT_PREV, - LON_PREV)
  return(df)
}

add_time_and_distance_to_go <- function(.trj, .mst = "ALDT", .time_unit = "sec"){
  df <- .trj
  
  df <- df |> 
    add_flight_distance() |> 
    dplyr::mutate(
      TIME_2GO = difftime(
          TIME[which(MST == .mst)], TIME, units = .time_unit)
    , DIST_2GO = DIST_FLOWN[which(MST == .mst) - DIST_FLOWN])
  
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
