#' (Gu)estimate the arrival runway based on alignment of landing trajectory points and extended runway centerline
#' 
#'spatial join (aka "overlay" with "within")
# to identify most likely ~ closest runway (extended centerline-string)
#'
#' @param .trj_pts 
#' @param .rwy_ctrl_ls 
#' @param .ctrl_offset 
#'
#' @return
#' @export
#'
#' @examples
estimate_rwy <- function(.trj_pts, .rwy_ctrl_ls, .ctrl_offset = 1000){
  # stop if not
  tmp <- sf::st_join( .rwy_ctrl_ls, .trj_pts
                  , join = sf::st_is_within_distance, dist = .ctrl_offset) %>%
    sf::st_drop_geometry() %>%        # keep just the "spatially joined" dataframe
    # na.omit() %>%                 # eliminate no hits
    dplyr::group_by(REF) %>%
    dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate( TOT   = sum(N, na.rm = TRUE)       # na.rm should not be necessary
            ,TRUST = N / TOT)                   # share in case several runways
  
  return(tmp)
}


#' Assign most likely arrival landing
#'
#' @param .rwy_hit_score 
#'
#' @return
#' @export
#'
#' @examples
#' #' \dontrun{
#' }
arrival_runway_assignment <- function(.rwy_hit_score){
  .rwy_hit_score |> 
    dplyr::arrange(desc(N)) |> 
    dplyr::mutate(
      COMMENT = paste0("landing runway: ", REF," trust-Level (hits: ", N, "): ", TRUST)) |> 
    dplyr::filter(row_number() == 1)
}


#' Assign landing runway
#'
#' @param .arr_trjs 
#' @param .arp 
#' @param .group_var 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
assign_landing_runway <- function(.arr_trjs, .arp = arp_egll, .rwy_centerline_ls, .upper_bound = 12 , .group_var = UID ,...){
  final_trjs <- .arr_trjs |>
    # omit flights close to ARP ~ remove potential surface hits -> min = 2!
    # find a better way to filter final 
    trrrj::filter_positions_at_range(c(.arp$LON, .arp$LAT), 2, .upper_bound, lon = LON, lat = LAT)
  
  final_trjs_pts <- final_trjs |> cast_latlon_to_pts()
  
  ldg_rwy <- final_trjs_pts |> 
    group_by({{.group_var}}) |> 
    group_modify(
      .f = ~ estimate_rwy(.x, .rwy_centerline_ls) |> 
             arrival_runway_assignment()
      ) |> ungroup()
  
  return(ldg_rwy)
}