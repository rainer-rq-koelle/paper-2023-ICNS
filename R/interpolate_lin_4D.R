#' Simple linear interpolation of 4D positions (e.g. CPF/CPR)
#'
#' @param .cpf
#' @param .source
#'
#' @return
#' @export
#'
#' @examples
interpolate_lin_4D <- function(.cpf, .time_step = "5 sec", .source = "CPF"){
  df <- .cpf  # to-do check and test

  # #----------- set UID, if not existing
  # uid_exists <- colnames(df) %in% "UID" %>% any()
  # if(!uid_exists) df <- df %>% dplyr::mutate(UID = SAM_ID)

  min_max_time = df %>%
    dplyr::filter(dplyr::row_number() %in% c(1, n())) %>%
    dplyr::summarise(start = dplyr::first(TIME), end = dplyr::last(TIME)) %>%
    dplyr::transmute(TIME = purrr::map2(start, end, seq, by = .time_step)) %>%
    tidyr::unnest(cols = TIME)

  df2 <- min_max_time %>%
    dplyr::full_join(df %>% dplyr::mutate(SOURCE = .source), by = "TIME") %>%
    dplyr::arrange(TIME) %>%
    #tidyr::fill(UID, .direction = "down") %>%
    # mutate(across(.cols = c(ALT), .fns = ~zoo::na.approx, seq, na.rm=FALSE))
    dplyr::mutate(
       ALT_B  = zoo::na.approx(ALT_B, x = TIME, na.rm = FALSE)
      ,LAT = zoo::na.approx(LAT, x = TIME, na.rm = FALSE)
      ,LON = zoo::na.approx(LON, x = TIME, na.rm = FALSE)
      ,SOURCE = if_else(is.na(SOURCE), "INP", SOURCE)
    )
  return(df2)
}
