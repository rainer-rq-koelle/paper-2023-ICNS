#' Check for level portions of flight
#'
#' @param .trj
#' @param VV_threshold
#' @param Time_threshold
#'
#' @return
#' @export
#'
#' @examples
check_level_flight <- function(.trj
                               , VV_threshold = 3/60   # FL/min, min = 60 seconds
                               , Time_threshold= 20    # seconds := min length of level
                               , .alt_var=FL
){
  Alt_threshold=VV_threshold*Time_threshold #FL
  ## wrapper for Sam's CDO/CCO level identification
  df <- .trj
  df <- df %>%
    mutate(SEQ_ID = row_number()) %>%
    rename(FLIGHT_LEVEL = {{.alt_var}}, TIME_OVER = TIME) %>%

    mutate(
      LEVEL=ifelse(SEQ_ID,
                   ifelse(abs((FLIGHT_LEVEL-lead(FLIGHT_LEVEL))/as.numeric(difftime(TIME_OVER, lead(TIME_OVER), units="secs")))<=VV_threshold, 1, 0),
                   ifelse(is.na(ifelse(abs(FLIGHT_LEVEL-lead(FLIGHT_LEVEL))<=Alt_threshold, 1, 0)), 0,
                          ifelse(abs(FLIGHT_LEVEL-lead(FLIGHT_LEVEL)) <= Alt_threshold, 1, 0))),
      BEGIN=ifelse(LEVEL==1 & (lag(LEVEL)==0 | TIME_OVER==min(TIME_OVER)), 1, 0),
      END=ifelse(LEVEL==0 & TIME_OVER!=min(TIME_OVER) & lag(LEVEL)==1, 1, 0)
    )
  ## re-wrap
  df <- df %>% rename({{.alt_var}} := FLIGHT_LEVEL, TIME = TIME_OVER)

}
