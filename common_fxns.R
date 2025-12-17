here_anx <- function(f = '', ...) {
  ### create file path to git-annex dir for project
  f <- paste(f, ..., sep = '/')
  f <- stringr::str_replace_all(f, '\\/+', '/')
  f_anx <- sprintf('/home/shares/usgs-cap-rangeshifts/%s', f)
  return(f_anx)
}

### Define aggregation function
agg_scenario <- function(s, df, scenario = 'future') {
  ### for species s, filter historical map files from df; rasterize; mean

  if(scenario == 'future') { ### filter to non-Historical
    s_df <- df %>%
      filter(spp == s & !str_detect(f, 'Historical'))
  } else { ### else filter to Historical
    s_df <- df %>%
      filter(spp == s & str_detect(f, 'Historical'))
  }
  s_map <- rast(s_df$f) %>%
    crop(us_extent) %>%
    sum(na.rm = TRUE)
  s_map <- s_map / nrow(s_df)
  return(s_map)
}

extract_jurisdictions <- function(spp, future_r, hist_r, juris_r) {
  ### spp <- spp_vec[1]
  ### future_r = future_rast; hist_r = hist_rast; juris_r = states_rast
  s_f_r <- future_r[[names(future_r) == spp]]
  s_h_r <- hist_r[[names(hist_r) == spp]]
  s_df <- data.frame(values(s_h_r),
                     values(s_f_r),
                     values(juris_r)) %>%
    setNames(c('historical', 'future', 'id')) %>%
    filter(!is.na(id)) %>%
    filter(!is.na(historical) | !is.na(future)) %>%
    group_by(id) %>%
    summarize(n_historical = sum(historical > .25),
              n_future = sum(future > .5))

  return(s_df)
}

# Write a function to get the most recent, global iucn assessment id for each species
iucn_id <- function(api, genus, species){
  # get species info
  species_info <- assessments_by_name(api, genus, species)
  list_status <- species_info %>%
    filter(scopes_code == 1 & latest == TRUE) #%>%
  #filter(year_published == max(year_published))
  return(list_status$assessment_id)
}
