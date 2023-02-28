# convert sequence of points to step linestrings
# inspiration from SO
# https://stackoverflow.com/questions/64594460/create-line-segments-from-gps-points-in-r-sf
# source_file <- 'INSERT_FILENAME_HERE'
#
# empty <- st_as_sfc("POINT(EMPTY)", crs = 4326)
#
# sf::st_read(source_file) %>%
#   # great circle distances
#   st_set_crs(4326) %>%
#   mutate(
#     geometry_lagged = lag(geometry, default = empty)
#   ) %>%
#   # drop the NA row created by lagging
#   slice(-1) %>%
#   mutate(
#     line = st_sfc(purrr::map2(
#       .x = geometry,
#       .y = geometry_lagged,
#       .f = ~{st_union(c(.x, .y)) %>% st_cast("LINESTRING")}
#     ))) -> track_lines

cast_latlon_to_step_ls <- function(.df, .crs = 4326){

  df <- .df %>%
    cast_latlon_to_pts(.drop_coord = FALSE)

  empty <- sf::st_as_sfc("POINT(EMPTY)", crs = .crs)

  steps <- df %>%
    dplyr::mutate(prev_geometry = dplyr::lag(geometry, default = empty)) %>%
    # drop the NA row created by lagging
    dplyr::slice(-1) %>%
    dplyr::mutate(
      line = st_sfc(purrr::map2(
              .x = geometry,
              .y = prev_geometry,
              .f = ~{
                  sf::st_union(c(.x, .y)) %>%
                  sf::st_cast("LINESTRING")
                }
              )
              )
    )
}
