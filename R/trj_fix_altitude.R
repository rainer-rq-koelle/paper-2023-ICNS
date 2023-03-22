#' Removing absurd altitudes when aircraft on ground
#'
#' @param .trj 
#'
#' @return
#' @export
#'
#' @examples
fix_altitude_heuristic <- function(.trj){
  fixed <- .trj |> 
    dplyr::mutate(
      ALT_B = dplyr::case_when(
        # capture off-sets for landing/rolling/blocking-in aircraft
          velocity < 90 & ALT_B > 5000 & !is.na(ALT_G) ~ ALT_G
        # capture off-sets for departing aircraft
        # or is.na(ALT_G)
        # TODO
      )
    )
  
  # landing trajectories have no altitude information towards the end
  # we need to probably identify standstill / gate-in
  # heuristic we fill downwards in time
  fixed <- fixed |> 
    tidyr::fill(ALT_B, .direction = "down")
  
  return(fixed)
}