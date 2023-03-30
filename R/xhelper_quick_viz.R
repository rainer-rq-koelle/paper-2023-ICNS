viz_check_DAO <- function(.trj, .apt_box = airport_box){
  # check dim of box
  bbox = sf::st_bbox(.apt_box)
  
  plt <- .trj |> 
    ggplot() + 
    geom_sf(data = .apt_box, fill = NA, color = "blue") + 
    geom_point(data = .trj, aes(x = LON, y = LAT, color = TIME)) + 
    coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE)
  return(plt)
}

viz_check_vertical <- function(.trj){
  plt <- .trj |> 
    ggplot() +
    geom_point(aes(x = TIME, y = ALT_B))
  return(plt)
}
