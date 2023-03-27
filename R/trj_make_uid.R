#' assign an UID for the study by attributing ICAO24, FLTID, DOF, and LEG
#'
#' @param .trj_with_legs 
#' @param .dof 
#'
#' @return
#' @export
#'
#' @examples
make_uid <- function(.trj_with_legs, .dof){
  # strip off hyphons from date
  dof <- .dof |> gsub(pattern = "-", replacement = "")
  my_trjs <- .trj_with_legs |> 
    dplyr::mutate(UID = paste(ICAO24, dof, LEG, FLTID, sep = "-")) |> 
    dplyr::select(UID, dplyr::everything())
  return(my_trjs)
}