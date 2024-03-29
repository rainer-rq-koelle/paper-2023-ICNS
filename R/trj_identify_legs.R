#' Identify legs / gaps in a trajectory
#'
#' Label legs (segments) of a trajectory (time ordered sequence of ICAO24)
#' addresses.
#'
#' @param .trjs
#' @param .grp_var
#' @param .max_gap in minutes
#'
#' @return
#' @export
#'
#' @examples
identify_trajectory_legs <- function(.trjs, .grp_var=ICAO24, .max_gap=10){
  df <- .trjs %>%
    dplyr::group_by({{.grp_var}}) %>%
    dplyr::arrange(TIME, .by_group = TRUE) %>%
    dplyr::mutate(
      SEQ_ID = dplyr::row_number()
      , STEP = difftime(TIME, dplyr::lag(TIME, default = dplyr::first(TIME)), units = "sec")
      #-- add grouping for trajectory segments
      ,  LEG = cumsum( STEP >= .max_gap * 60)    ) %>%
    ungroup()
  return(df)
}
