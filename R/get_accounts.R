get_accounts <- function(at_date = lubridate::now()){
  
  accs <- readr::read_csv("inst/extdata/prc_diplomats_twitter_historical.csv")
  
  out_df <- 
    accs %>% 
    dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
    dplyr::select(1:8)
  
  out_df[is.na(out_df)] <- 
    ""
  
  return(out_df)
  
}

#write.csv(out_df,"inst/extdata/prc_diplomats_twitter_latest.csv")