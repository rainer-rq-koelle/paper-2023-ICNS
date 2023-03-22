#' Utility functions to discover and download one day (Monday) data from Opensky Network
#'
#' @return table of available dump dates
#' @export
#'
#' @examples
#' \dontrun{}
osn_list_dumps <- function(.url = "https://opensky-network.org/datasets/states/"){

  url <- .url
  # scrape page
  dump_page <- rvest::read_html(url)

  # extract available / prepared dumps
  dumps <- dump_page %>%
    rvest::html_element("tbody") %>%
    rvest::html_table()

  # filer Monday dumps
  dumps <- dumps %>%
    dplyr::filter(grepl(pattern = "^2[0-9]{3}", x = X1))    # filter 2020-xx-yy etc

  return(dumps)
}


construct_dump_download_links <- function(.date, .url = "https://opensky-network.org/datasets/states/"){

  url <- .url    # todo check for web address, etc
  url_date <- paste0(
     url
    ,.date
  )

  # scrape page
  hour_chunks <-
    rvest::read_html(url_date) %>%
    rvest::html_element("tbody") %>%
    rvest::html_table() %>%
    dplyr::filter(grepl(pattern = "[0-9]{2}", x = X1))

  # links
  urls <- paste0(url_date, "/", dplyr::pull(hour_chunks, X1))

  return(urls)
}


osn_download_hour_dump <- function(.url_date_hr, .dest_path = NULL){

  # check for tar-file exists
  target <-
    rvest::read_html(.url_date_hr) %>%
    rvest::html_element("tbody") %>%
    rvest::html_table() %>%
    dplyr::filter(grepl(pattern = ".csv.tar", x = X1))

  if (nrow(target) != 1) {
    message(paste0("No download for ", .url_date_hr))
    # df <- tibble::tibble(COMMENT = "No data dump or multiple target files.")
  } else{
    message(paste0("Download tar exists."))

    download_url = paste0(.url_date_hr, "/", target$X1)

    if (is.null(.dest_path)) {
      destination <- basename(download_url)
    } else{
      destination <- paste0(.dest_path, "/", basename(download_url))
    }

    download.file(
        url      = download_url
      , destfile = destination
      , method   = "libcurl"
   ## https://stackoverflow.com/questions/48927391/trouble-downloading-tar-in-r
      , mode     = "wb"      # need to be set to "write binary" for tars!
    )
  } #------------ end else := download
}
