add_landing_time_to_nested_data <- function(.trj_arr, .uid, .thr, .rwys){
#add_landing_time_to_nested_data <- function(.input_list, .rwys){
  message(paste0("processing ", .uid , " with ", .thr))
  this_trj <- .trj_arr |>
    dplyr::mutate(LUID = .uid, RWY = .thr) |>
    dplyr::group_by(LUID) |>
    dplyr::group_modify(.f = ~assign_threshold_time2(.x, .rwys) ) |>
    dplyr::ungroup() |>
    dplyr::select(-LUID)
}
