#' Clean weird stuff in FLTIDs of OSN
#'
#' @param .trjs 
#' @param .fltid default FLTID, set as appropriate
#' @param ... not sure yet ... could be of help later
#'
#' @return corrected FLTIDs
#' @export
#'
#' @examples
fix_invalid_fltid <- function(.trjs, .fltid = FLTID,...){
  fixed <- .trjs   # to-do ... check what you get
  # remove fltids with space
  fixed <- fixed |> 
    dplyr::mutate(
      # label and remove FLTIDs with empty space(s)
       FIXCHECK = stringr::str_count({{.fltid}}, pattern = "\\s+")
      ,{{.fltid}} := ifelse(FIXCHECK == 1, NA_character_, {{.fltid}})
      )
  
  # fixes should leave only valid FLTIDs and NAs
  # NAs can be filled
  fixed <- fixed |> 
    tidyr::fill({{.fltid}}, .direction = "downup") |> 
  # remove fixing label
    dplyr::select(-FIXCHECK)
}

