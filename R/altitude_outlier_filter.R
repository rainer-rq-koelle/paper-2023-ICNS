#' Smooth altitude
#' 
#' Kudos to Sam
#'
#' @param df 
#' @param KernelSize 
#' @param Fill 
#' @param IntResults 
#' @param .id 
#' @param .alt_var 
#'
#' @return
#' @export
#'
#' @examples
Alt_Outlier_filter=function(df, KernelSize = 5, Fill = TRUE, IntResults = FALSE, .id = ICAO24, .alt_var=FL_B){
  df1 = dplyr::group_by(df, {{.id}}) %>%
    dplyr::mutate(
      ALT_med  = zoo::rollmedian({{.alt_var}}, KernelSize, fill=NA),
      sq_eps   = ({{.alt_var}}-ALT_med)^2,
      sigma    = sqrt(zoo::rollmean(sq_eps, KernelSize, fill=NA)),
      Outlier   = ifelse(sq_eps>sigma, 1, 0))
  # Choose if the outliers need to be filled by the median altitude
  if (Fill==TRUE) {
    df1 = dplyr::mutate(df1, {{.alt_var}} := ifelse((Outlier==0 | is.na(Outlier)), {{.alt_var}}, ALT_med))
  }
  # Choose if the intermediate results need to be returned in the dataframe
  if (IntResults==FALSE) {
    df1=dplyr::select(df1, -ALT_med, -sq_eps, -sigma)
  }
  return(df1)
}
