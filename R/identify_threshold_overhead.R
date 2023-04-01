identify_threshold_overhead <- function(.final, .my_thr ,...){
  trj <- .final |> 
    trj_add_target_bearing(.my_thr) |> 
    dplyr::mutate(
       OVERHEAD = between(as.numeric(DIST_THR), -100, 100)
      , BRG_CHG = ! abs(lag(BRG) - BRG) < 25
      , OVR_THR = OVERHEAD & BRG_CHG) |> 
    dplyr::filter(OVR_THR)
}

identify_threshold_ahead <- function(.final, .my_thr ,...){
  trj <- .final |> 
    dplyr::filter(ALT_B > arp$ELEV + 500) |> 
    trj_add_target_bearing(.my_thr)
  
  # add threshold
  # thr <- select(.my_thr, LAT, LON)
  # trj <- bind_rows(trj, thr) |>
  #   dplyr::mutate(NODE_ID = dplyr::lag(NODE_ID) + 1) |> 
  #   tidyr::fill(UID, ICAO24) |> 
  #   trj_add_target_bearing(.my_thr)
}

identify_threshold_ahead2 <- function(.final, .my_thr ,...){
  trj <- .final |> 
    dplyr::filter(ALT_B > .my_thr$ELEV + 500) |> 
    trj_add_target_bearing(.my_thr)
}