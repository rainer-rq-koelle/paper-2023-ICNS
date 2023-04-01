#' Determine threshold time for arrival trajectories
#' REVISED VERSION TO WORK WITH REPLICATE WORKFLOW
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
assign_threshold_time2 <- function(.trj_latlon_sf, .rwys, ...){
  trj_arr <- .trj_latlon_sf
  my_thr  <- .rwys |> dplyr::filter(REF == unique(trj_arr$RWY))
  # trim payload and add sequence id
  trj_arr <- trj_arr |> # select(UID, TIME, ICAO24, LAT, LON, ALT_B)  |> 
    rowid_to_column("NODE_ID") |> 
    mutate(MST = NA_character_) 
  
  if(!is(trj_arr, "sf")){
  # stop("Y has to be a spatial object ")
    trj_arr <- trj_arr |> cast_latlon_to_pts(.drop_coord = FALSE)
  }  
  
  # ldg_rwy <- trj_arr |> 
  #   assign_landing_runway(.arp = arp) |> 
  #   inner_join(rwys, by = "REF")
  # my_thr  <- ldg_rwy |> select(UID, REF, LAT, LON, COMMENT) |>
  my_thr <- my_thr |> 
    cast_latlon_to_pts(.drop_coord = FALSE)

  # # check if we have a threshold overflight
  ovr_thr <- identify_threshold_overhead(trj_arr, my_thr)
  if(nrow(ovr_thr) > 0){
    message("THRESHOLD")
    thr_row <- ovr_thr |> filter(DIST_THR == min(DIST_THR))
    trj_arr[thr_row$NODE_ID,"MST"] <- "ALDT"
    trj_arr[thr_row$NODE_ID,"MST_REF"] <- my_thr$REF
  }else{
     message("NEED TO WORK ON LANDING TIME")
    thr_row <- trj_arr |>
      identify_threshold_ahead2(my_thr) |>
      estimate_threshold_time(my_thr)
    
    if(nrow(thr_row) < 1) { # ============= catch error ----------------
      message("NO THR")
      thr_row <- tibble(NODE_ID = 999, MST = "ALDT FAIL", MST_REF = my_thr$REF)
      trj_arr <- trj_arr |> dplyr::bind_rows(thr_row)
      #=================================== end error handling ----------
    }else{
  
    thr_row$NODE_ID <- thr_row$NODE_ID +1       # increase node counter
    thr_row$MST     <- "ALDT"
    thr_row$MST_REF <- my_thr$REF
    
    trj_arr <- trj_arr |>
        filter(NODE_ID < thr_row$NODE_ID)  |>     # truncate
        bind_rows(thr_row)
    }
  } #------------ end else = approx for landing time
  
  return(trj_arr <- trj_arr |> select(NODE_ID:MST, MST_REF))
}