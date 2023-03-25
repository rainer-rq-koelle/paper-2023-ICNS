#' Determine threshold time for arrival trajectories
#'
#' Work out homing and coasting of arrival trajectories. Identify overhead
#' threshold or ahead of threshold in case of low coverage.
#' Estimate time over threshold based on most recent position updates (and
#' derived approach speed).
#'
#' @param .trj_latlon_sf 
#' @param .my_thr 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
assign_threshold_time <- function(.trj_latlon_sf, .my_thr, ...){
  trj_arr <- .trj_latlon_sf
  my_thr  <- .my_thr
  # trim payload and add sequence id
  trj_arr <- trj_arr |> select(UID, TIME, ICAO24, LAT, LON, ALT_B)  |> 
    rowid_to_column("NODE_ID") |> 
    mutate(MST = NA_character_)
  
  ldg_rwy <- trj_arr |> assign_landing_runway(.arp = arp) |> 
    inner_join(rwys, by = "REF")
  my_thr  <- ldg_rwy |> select(UID, REF, LAT, LON, COMMENT) |> 
    cast_latlon_to_pts(.drop_coord = FALSE) 
  
  # check if we have a threshold overflight
  ovr_thr <- identify_threshold_overhead(trj_arr, my_thr)
  if(nrow(ovr_thr) > 0){
    message("THRESHOLD")
    thr_row <- ovr_thr |> filter(DIST_THR == min(DIST_THR)) 
    trj_arr[thr_row$NODE_ID,"MST"] <- "ALDT"
    trj_arr[thr_row$NODE_ID,"MST_REF"] <- my_thr$REF
  }else{
    message("NEED TO WORK ON LANDING TIME")
    thr_row <- trj_arr |> 
      identify_threshold_ahead(my_thr) |> 
      estimate_threshold_time(my_thr)
    thr_row$NODE_ID <- thr_row$NODE_ID +1       # increase node counter
    thr_row$MST     <- "ALDT"
    thr_row$MST_REF <- my_thr$REF
    trj_arr <- trj_arr |> 
      filter(NODE_ID < thr_row$NODE_ID)  |>     # truncate
      bind_rows(thr_row)             
  }
  return(trj_arr <- trj_arr |> select(NODE_ID:MST, MST_REF))
}