# RUNWAY UTILITY FUNCTIONS

# setup sf objects
#arp_pt   <- arp_egll |> cast_latlon_to_pts()
#rwys_pts <- rwys_egll |> cast_latlon_to_pts()

# utility function to coerce data frame of runway points to sf linestring
cast_rwy_ls <- function(.rwys_df, .rwy_name = NAME){
  rwys_ls  <- .rwys_df |> 
    dplyr::group_by({{.rwy_name}}) |>
    dplyr::group_modify(.f = ~ cast_latlon_to_ls(.) ) |> 
    sf::st_as_sf()
  return(rwys_ls)
}

# utility function to create centerline
rwy_ctr_line <- function(.rwy_df = aip, .ctrl_length = 10000){
  df <- .rwy_df %>% 
    filter(REF != "ARP") %>% 
    select(REF, NAME, LAT, LON) %>% 
    group_by(NAME) %>% 
    mutate( LAT2 = case_when(!is.na(lag(LAT))  ~ lag(LAT)
                             ,!is.na(lead(LAT)) ~ lead(LAT))
            ,LON2 = case_when(!is.na(lag(LON))  ~ lag(LON)
                              ,!is.na(lead(LON)) ~ lead(LON))
    ) %>% 
    # calculate "reverse" runway bearing with geosphere::bearingRhumb
    mutate( RBRG  = geosphere::bearingRhumb(p1 = cbind(LON2, LAT2), p2 = cbind(LON, LAT))
    )
  
  # determine "endpoint" of extended centerline at d = 10000 meters
  tmp <- with(df, geosphere::destPointRhumb(cbind(LON, LAT), b= RBRG, d = .ctrl_length)) %>% 
    as_tibble() %>% rename(LON3 = lon, LAT3 = lat)
  
  # combine and return
  df <- df %>% bind_cols(tmp)
  return(df)
}

# utility function to coerce extension points
rwy_ctr_line_pts <- function(.rwy_ctr_line, .debug = FALSE){
  # could not get pivot_longer work with multiple cols
  tmp1 <- .rwy_ctr_line %>% select("REF":"LON")
  
  # include opposite runway threshold
  tmp2 <- .rwy_ctr_line %>% select("REF","NAME", "LAT2","LON2") %>%
    rename(LAT = LAT2, LON = LON2)
  
  # centerline end point determined beforehand
  tmp3 <- .rwy_ctr_line %>% select("REF","NAME", "LAT3","LON3") %>%
    rename(LAT = LAT3, LON = LON3)
  
  if(.debug == TRUE){ 
    df <- bind_rows(tmp1, tmp2, tmp3)
  }else{
    df <- bind_rows(tmp1, tmp3)
  }
  
  df <- df %>% arrange(REF, NAME)
  return(df)  
}

cast_rwy_ctr_line_ls <- function(.rwys_df){
  .rwys_df |> 
    rwy_ctr_line() |> 
    rwy_ctr_line_pts() |> 
    cast_latlon_to_pts() |> 
    cast_pts_to_ls(.group_var = REF)
}


# utility function to build box around buffered runway thresholds
airport_threshold_box <- function(.rwys_df, .thr_buffer = 500, ...){
  # cast to sf-pts and add buffer
  rwys_pts_buf <- .rwys_df |> cast_latlon_to_pts() |> 
    sf::st_buffer(dist = .thr_buffer)
  # combine and add convex hull to encompass all thresholds
  rwys_box <- rwys_pts_buf |> sf::st_union() |> sf::st_concave_hull()

  return(rwys_box)
}