#' Title
#'
#' @param .seen_at_airport 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
heuristic_DAO <- function(.seen_at_airport, ...){
  DAO <- .seen_at_airport |> 
    dplyr::mutate(
      DAO = case_when(
         MIN_TIME_ALT < MAX_TIME_ALT ~ "D"
        ,MIN_TIME_ALT > MAX_TIME_ALT ~ "A"
        ,MED_ALT > 5000 ~ "O"
        ,TRUE ~ NA_character_
      )
      )
}