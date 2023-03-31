#' Utility function to confirm arrival phase at an airport.
#'
#' The time-ordered set of trajectories is spatially intersected with a polygon
#' around the airport (aka airport box).
#'
#' @param .df
#' @param .apt_box
#'
#' @return
#' @export
#'
#' @examples
confirm_arrival_at <- function(.df, .apt_box, .time_window = 500, ...){
  df <- .df %>%
    # extract last (aka oldest) pos reports
    dplyr::mutate(last_4d = map(
      .x = data
      ,.f = ~ filter(.x, TIME >= max(TIME) - .time_window)
        ) # pam
    )  |> 
    dplyr::mutate(ARR = map(
      .x = last_4d
      ,.f = ~ cast_latlon_to_ls(.x) %>%
        st_intersects(.apt_box, sparse = FALSE))
    ) %>%
    tidyr::unnest(ARR) |> 
    dplyr::mutate(N_LAST = map(last_4d, nrow)) |> 
    tidyr::unnest(N_LAST) |> 
    ##------------ add runway estimation
    dplyr::mutate(
      RWY_SCORE = map(
            .x = last_4d
          , .f = ~ estimate_rwy(
                    .x |> cast_latlon_to_pts()
                  , rwy_centerline_ls
                  ) |> 
            arrival_runway_assignment()
          )
      ) |> mutate(RWY = map(.x = RWY_SCORE, .f = ~ .x$REF)) |> tidyr::unnest(RWY) |> 
    mutate(
      RWY_TRUST = map(
                .x = RWY_SCORE
              , .f = ~ .x$TRUST)
      ) |> tidyr::unnest(RWY_TRUST)
  
  df <- df %>% 
    dplyr::mutate(
      ARR = as.logical(ARR)
      )
  return(df)
}
