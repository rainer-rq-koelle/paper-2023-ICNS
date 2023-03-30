make_study_arrival_trajectories2 <- function(.trjs, .rwys){
  this_itr <- .trjs |> dplyr::distinct(UID, RWY)
  message("\n", this_itr$UID)
  app_trj <- .trjs |> dplyr::filter(UID == this_itr$UID)
  app_thr <- .rwys |> dplyr::filter(REF == this_itr$RWY)
  
  # estimate threshold (aka landing time)
  thr_row <- estimate_threshold_time(app_trj, app_thr) |> 
    dplyr::select(UID, ICAO24, FLTID, TIME, LAT, LON ) |> 
    dplyr::mutate(MST = "THR", MST_LABEL = this_itr$RWY)
  
  # append threshold row and trim trajectory
  app_trj <- app_trj |> 
    dplyr::bind_rows(thr_row) |> 
    dplyr::select(UID, ICAO24, FLTID, TIME, LAT, LON, ALT_B, MST, MST_LABEL) |> 
    dplyr::arrange(TIME) |> 
    dplyr::rowid_to_column("SEQ_ID") |> 
    # trim to threshold
    dplyr::filter(TIME <= thr_row$TIME)
  
  # add "distance-to-go" and "time-to-fly" 
  app_trj <- app_trj |> 
    add_flight_distance() 
  # |> 
  #   dplyr::mutate(
  #      DIST_TO_THR = thr_row$DIST_FLOWN - DIST_FLOWN
  #     ,TIME_TO_THR = difftime(thr_row$TIME, TIME, unit = "sec")
  #   )
  return(app_trj)
}

safe_make_study_arrival_trajectories2 <- purrr::safely(.f = make_study_arrival_trajectories2, otherwise = "ERROR" )

safer_make_study_arrival_trajectories2 <- purrr::possibly(.f = make_study_arrival_trajectories2, otherwise = "ERROR")