#' Set up milestone table for study
#'
#' @param .trjs set of trajectories UID & LAT/LON positions
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
initialise_milestone_table <- function(.trjs, ...){
  # ensure time ordering
  trjs <- .trjs |> 
    dplyr::arrange(TIME, .by_group = TRUE)
  # first and last trajectory point
  start <- trjs |> dplyr::slice(1, .by = UID) |> 
    dplyr::mutate(MST = "START")
  stop  <- trjs |> dplyr::slice(n(), .by = UID) |> 
    dplyr::mutate(MST = "STOP")
  
  mst <- dplyr::bind_rows(start, stop) |> 
    dplyr::select(UID, TIME, FLTID, LAT, LON, ALT_B, MST) |> 
    dplyr::arrange(UID, TIME)
  return(mst)
}