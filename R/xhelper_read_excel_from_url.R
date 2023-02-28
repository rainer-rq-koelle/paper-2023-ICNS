## fails miserably when trying to download excel sheet from github
## need to recode github download urls!

#' Downlad an excel sheet
#'
#' @param .url 
#' @param .sheet 
#'
#' @return
#' @export
#'
#' @examples
#' #' \dontrun{
#' fn <- "https://ansperformance.eu/download/xls/Taxi-Out_Additional_Time.xlsx"
#' ds <- download_excel_from_url(fn, .sheet = "DATA)
#' egll
#' }
read_excel_from_url <- function(.url, .sheet=NULL){
  httr::GET(.url, httr::write_disk(tf <- tempfile(  #tmpdir = "./tmp", 
    fileext = ".xlsx")))
  ds <-  readxl::read_excel(tf, sheet = .sheet)
}