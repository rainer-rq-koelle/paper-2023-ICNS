#' Removing absurd altitudes when aircraft on ground
#'
#' @param .trj 
#'
#' @return
#' @export
#'
#' @examples
fix_altitude_heuristic <- function(.trj, .v_to = 50){
  fixed <- .trj |> 
  # flights are airborne at velocity > 
    dplyr::filter(velocity >= .v_to) |> 
  # remove off-set for landing flights
    dplyr::filter(!(velocity < 90 & ALT_B > 5000))
  
  # landing trajectories have no altitude information towards the end
  # we need to probably identify standstill / gate-in
  # heuristic we fill downwards in time
  # fixed <- fixed |> 
  #   tidyr::fill(ALT_B, .direction = "down")
  
  return(fixed)
}