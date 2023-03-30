#' Estimation of time over threshold based on last positions before threshold
#'
#' @param .trjs_latlon_sf 
#' @param .my_thr 
#'
#' @return
#' @export
#'
#' @examples
estimate_threshold_time <- function(.trjs_latlon_sf, .my_thr){
  this_trj <- .trjs_latlon_sf
  
  # decode runway identifier
  RWY <- .my_thr$REF
  rwy_to_number <- function(.rwy){
    rwy_dir <- gsub("\\D","", .rwy) # strip non-digets and replace with empty
    rwy_dir <- ((as.numeric(rwy_dir) * 10) + 360) %% 360
  }
  # check if BEARING and DISTANCE to threshold are already calculated
  if(!"DIST_THR" %in% colnames(this_trj)){
    this_trj <- this_trj |> 
      dplyr::mutate(
        DIST_THR = geosphere::distHaversine(
                cbind(LON, LAT)
              , cbind(.my_thr$LON,.my_thr$LAT)
              )
        )
  }
  if(!"BRG_THR" %in% colnames(this_trj)){
    this_trj <- this_trj |> 
      dplyr::mutate(
        BRG_THR = geosphere::bearingRhumb(cbind(LON, LAT), cbind(.my_thr$LON,.my_thr$LAT)))
  }
  
  last_pos <- this_trj %>%
    mutate(
             BEFORE  = abs(BRG_THR - rwy_to_number(RWY)) < 30
           , COASTG  = DIST_THR == lag(DIST_THR)
           , V_GND   = (lag(DIST_THR) - DIST_THR) / as.numeric(TIME - lag(TIME))
           , V_GND_SM= zoo::rollmean(V_GND, k = 9, fill = NA, align = "right")
    ) 
  
   any_costing <- sum(last_pos$COASTG == TRUE, na.rm = TRUE) #; message(paste0("ONLY AHEAD: "), any_costing) 
   # no costing
   if(any_costing > 1){
     last_pos <-  last_pos %>%
    # cut-off any position after the start of coasting
    # or if there are positions post threshold ~ BRG <> landing direction
     filter(COASTG == FALSE & BEFORE == TRUE )
   }
  
  # check length / duration of last positions
  last_15 <- last_pos %>% tail(n = 15)
  avg_final_app_speed <- mean(last_15$V_GND_SM, na.rm = TRUE) |> as.numeric()
  avg_final_app_speed <- avg_final_app_speed * 0.9   # assume 10% for flare
  
  thr_row <- last_15 |> dplyr::slice_tail(n = 1) |> 
    mutate( LAT = .my_thr$LAT, LON = .my_thr$LON
           ,TIME = TIME + ceiling((DIST_THR |> as.numeric()) / avg_final_app_speed)  # round up for complete secs
           ,DIST_THR = 0                                           # @ threshold!
    )
}
