my_pairwise_spacing <- function(.lead_uid, .trail_uid, .t_trail, .mickey = mickey, .bins,...){
  
  leader <- .mickey |> 
    dplyr::filter(UID == .lead_uid) |> 
    dplyr::select(UID, RWY2 = RWY, study_trj) |> 
    tidyr::unnest(study_trj) |> 
    add_bin_mins(.bins) |> 
    dplyr::select(UID, RWY, TIME, TIME_2GO, DIST_2GO, H3_BIN, TIME_MIN, TIME_Q5, DIST_MIN, DIST_Q5)|> 
    interpolate_lin_spacdev()
  
  trailer <- .mickey |> 
    dplyr::filter(UID == .trail_uid) |> 
    dplyr::select(UID, RWY2 = RWY, study_trj) |> 
    tidyr::unnest(study_trj) |> 
    add_bin_mins(.bins) |> 
    dplyr::select(UID, RWY, TIME, TIME_2GO, DIST_2GO, H3_BIN, TIME_MIN, TIME_Q5, DIST_MIN, DIST_Q5) |> 
    interpolate_lin_spacdev()
  
  # do time-warp for trailer and
  # rename variables
  trailer <- trailer |>
    dplyr::mutate(TIME_WARP = TIME - .t_trail) |>
    dplyr::rename(
        SEQ_UID = UID, SEQ_DIST_2GO = DIST_2GO, SEQ_TIME_2GO = TIME_2GO
       ,SEQ_TIME = TIME
       ,SEQ_H3_BIN = H3_BIN
       ,SEQ_MIN_DIST_2GO   = DIST_MIN, SEQ_MIN_TIME_2GO   = TIME_MIN
       ,SEQ_MIN_DIST_2GO_Q = DIST_Q5,  SEQ_MIN_TIME_2GO_Q = TIME_Q5
       )
  
  # join and calc spacing 
  spacedev <- leader |>
    dplyr::left_join(trailer, by = c("TIME" = "TIME_WARP", "RWY")) |> 
    dplyr::mutate(  
             MIN_TIME_OFF   = TIME_MIN - SEQ_MIN_TIME_2GO
           , MIN_TIME_OFF_Q = TIME_Q5  - SEQ_MIN_TIME_2GO_Q
           , MIN_DIST_OFF   = DIST_MIN - SEQ_MIN_DIST_2GO
           , MIN_DIST_OFF_Q = DIST_Q5  - SEQ_MIN_DIST_2GO_Q
           , DIST_OFF       = DIST_2GO - SEQ_DIST_2GO
           , STEP = max(dplyr::row_number()) - dplyr::row_number()
           ) |> 
    dplyr::select(UID, SEQ_UID, STEP
           , MIN_TIME_OFF, MIN_TIME_OFF_Q
           , MIN_DIST_OFF, MIN_DIST_OFF_Q
           , DIST_OFF
           , everything())
  return(spacedev)
}


# helper function to add bin info to postion
add_bin_mins <- function(.trj, .bins){
  trj <- .trj |>
    dplyr::left_join(.bins    # |> select(H3_BIN, RWY, MIN_DIST_2GO, MIN_TIME_2GO)
                    , by = c("RWY", "H3_BIN")
                    )
return(trj)
}

# helper interpolate on a 1-sec basis

interpolate_lin_spacdev <- function(.spacdev_data, .time_step = "1 sec"){
  df <- .spacdev_data  # to-do check and test
  
  min_max_time = df |>
    dplyr::filter(dplyr::row_number() %in% c(1, n())) |>
    dplyr::summarise(start = dplyr::first(TIME), end = dplyr::last(TIME)) |>
    dplyr::transmute(TIME = purrr::map2(start, end, seq, by = .time_step)) |>
    tidyr::unnest(cols = TIME)
  
  df2 <- min_max_time |>
    dplyr::full_join(df, by = "TIME") |>
    dplyr::arrange(TIME)  |> 
    tidyr::fill(UID, RWY, .direction = "down") |> 
    dplyr::mutate(
        DIST_2GO  = zoo::na.approx(DIST_2GO, x = TIME, na.rm = FALSE)
       ,TIME_2GO  = zoo::na.approx(TIME_2GO, x = TIME, na.rm = FALSE)
       ,TIME_MIN  = zoo::na.approx(TIME_MIN, x = TIME, na.rm = FALSE)
       ,TIME_Q5   = zoo::na.approx(TIME_Q5,  x = TIME, na.rm = FALSE)
       ,DIST_MIN  = zoo::na.approx(DIST_MIN, x = TIME, na.rm = FALSE)
       ,DIST_Q5   = zoo::na.approx(DIST_Q5,  x = TIME, na.rm = FALSE)
     )
}