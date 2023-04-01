#' Check for orphans not captured during data cleaning
#' 
#' Orphans are characterised by own UID and thus sample size equalling 
#' "last points" size
#'
#' @param .df_nested 
#'
#' @return
#' @export
#'
#' @examples \dontrun{}
check_arrival_orphan <-  function(.df_nested){
  .df_nested |> 
    dplyr::mutate(
      N_DATA = map(
              .x = data
            , .f = ~ nrow(.x))
      ) |> 
    tidyr::unnest(N_DATA) |>
    # check whether "last points" and data are of same size
    dplyr::mutate(ORPHAN = N_DATA == N_LAST)
}