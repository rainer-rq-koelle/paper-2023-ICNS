#' Title
#'
#' @param .df 
#'
#' @return
#' @export
#'
#' @examples
clean_osn_statevectors <- function(.df){
  clean_states <- .df |> 
    make_nice_names_osn() |> 
    mutate(across(.cols = c("ALT_B","ALT_G"), .fns = coerce_meter_to_feet))
}