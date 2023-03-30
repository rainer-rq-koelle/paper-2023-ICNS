icao24_in_polygon <- function(.ds, .poly, .id = ICAO24){
  # determine bounding box for polygon
  this_box <- sf::st_bbox(.poly)
  # pre-filter with dplyr - cut bounding box
  icao24s <- .ds |> 
    dplyr::filter(
      LAT > this_box$ymin, LAT < this_box$ymax
      ,LON > this_box$xmin, LON < this_box$xmax, 
    )
  # get ids seen
  icao24s <- icao24s |> 
    dplyr::summarise(
      N = n(), N_ID = length(unique(FLTID))
      , MIN_TIME = min(TIME,na.rm = TRUE)
      , MIN_TIME_ALT = ALT_B[which(TIME == MIN_TIME)]
      , MAX_TIME = max(TIME, na.rm = TRUE)
      , MAX_TIME_ALT = ALT_B[which(TIME == MAX_TIME)]
      , MIN_ALT = min(ALT_B, na.rm = TRUE)
      , MED_ALT = median(ALT_B, na.rm = TRUE)
      , MAX_ALT = max(ALT_B, na.rm = TRUE)
      , .by = {{.id}})
}