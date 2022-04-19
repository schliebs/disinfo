library(tidyverse)

# todo: suppress warnings being printed
get_accounts <- function(country,group){
  
  at_date = lubridate::now()
  
  if("CN" %in% country & "diplomats" %in% group){
    
    at_date <- lubridate::today()
    accs_cn <- readr::read_csv("inst/extdata/cn/cn_diplomats_twitter_historical.csv")
    labs_cn <- readr::read_csv("inst/extdata/cn/platform_labels_twitter_cn_dips.csv")
    dips_cn <- readr::read_csv("inst/extdata/cn/twitter_cn_dips.csv")
    
    out_df <- 
      accs_cn %>% 
      dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
      dplyr::select(1:9) %>% select(-user_id)
    out_df$ISO2[out_df$country == "Namibia"] <- "NA"
    
    res <- 
      out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
      left_join(labs_cn %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
      left_join(dips_cn %>% mutate(match = str_to_lower(screen_name)), by = "match") %>% select(-match,-date_scraped)
    
    print(paste0(nrow(res)," Chinese diplomats and government accounts. Twitter transparency labels as of ",labs_cn$date_scraped[1],". Account data as of ", dips_cn$date_scraped[1],"."))
    
    return(res)
  }
  
  if("RU" %in% country & "diplomats" %in% group){
    
    at_date <- lubridate::today()
    accs_ru <- readr::read_csv("inst/extdata/ru/ru_diplomats_twitter_historical.csv")
    labs_ru <- readr::read_csv("inst/extdata/ru/platform_labels_twitter_ru_dips.csv")
    dips_ru <- readr::read_csv("inst/extdata/ru/twitter_ru_dips.csv")
    
    out_df <- 
      accs_ru %>% 
      dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
      dplyr::select(1:9) %>% select(-user_id)
    out_df$ISO2[out_df$country == "Namibia"] <- "NA"
    
    res <- 
      out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
      left_join(labs_ru %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
      left_join(dips_ru %>% mutate(match = str_to_lower(screen_name)), by = "match") %>% select(-match,-date_scraped)
    
    print(paste0(nrow(res)," Russian diplomats and government accounts. Twitter transparency labels as of ",labs_cn$date_scraped[1],". Account data as of ", dips_cn$date_scraped[1],"."))
    
    return(res)
  }
  
}





