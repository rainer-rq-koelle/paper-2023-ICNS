#' Subset trajectory LAT/LON positions
#'
#' @param .df_latlon 
#' @param .outer_poly sf-object of outer rim 
#'
#' @return
#' @export
#'
#' @examples
spatial_subset_jormungand <- function(.df_latlon, .outer_poly, ...){
  # dplyr cut for bounding box is more efficient than sf one-by-one
  # determine bounding box for polygon
  this_box <- sf::st_bbox(.outer_poly)
  # pre-filter with dplyr - cut bounding box
  df <- .df_latlon |> 
    dplyr::filter(
       LAT > this_box$ymin, LAT < this_box$ymax
      ,LON > this_box$xmin, LON < this_box$xmax, 
    )
  
  # fine-tune filter with sf; spatial subsetting
  has_geometry <- is(.df_latlon, "sf")
  if(isFALSE(has_geometry)){
    df <- df |> cast_latlon_to_pts(.drop_coord = FALSE)
  }
   df <- df[.outer_poly, ]
}

establish_radial_jormungand <- function(.center_latlon, .radius_NM = 205, ...){
  center <- cast_latlon_to_pts(.center_latlon)
  circle <- center |> sf::st_buffer(dist = .radius_NM * 1852, nQuadSegs = 60 )
}