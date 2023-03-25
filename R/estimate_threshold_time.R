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
  
  # decode runway identifier
  RWY <- .my_thr$REF
  rwy_to_number <- function(.rwy){
    rwy_dir <- gsub("\\D","", .rwy) # strip non-digets and replace with empty
    rwy_dir <- ((as.numeric(rwy_dir) * 10) + 360) %% 360
  }
  
  last_pos <- .trjs_latlon_sf %>%
    mutate(#DIST_THR = distHaversine(cbind(LON, LAT), cbind(LON_THR,LAT_THR))
           #, BRG_THR = bearingRhumb( cbind(LON, LAT), cbind(LON_THR, LAT_THR))
           #,
             BEFORE  = abs(BRG - rwy_to_number(RWY)) < 30
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
