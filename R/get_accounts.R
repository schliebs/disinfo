get_accounts <- function(at_date = lubridate::now()){
  
  accs <- readr::read_csv("extdata/prc_diplomats_twitter_historical.csv")
  
  out_df <- 
    accs %>% 
    dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date)
  
  return(out_df)
  
}

#write.csv(out_df,"inst/extdata/prc_diplomats_twitter_2021-07-05.csv")