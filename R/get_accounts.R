library(tidyverse)

#' Get lists of Russian or Chinese Diplomats
#'
#' @param country currently supports "RU" and "CN" (or both: c("RU","CN"))
#' @param group currently supports "diplomats"
#' @param online whether most recent version of data should be pulled, as opposed to only loaded when package is installed. Requires internet connection.
#' @return returns data.frame with twitter handles and details on dipomats
#' @references tbc
#' @note tbc
#' @examples
#' options(tidyverse.quiet = TRUE)
#' library(tidyverse)
#' ru_accs <- 
#'   get_accounts(country = "RU",group = "diplomats")
#' 
#' head(ru_accs,10)
#' 
#' ru_accs %>%
#'   group_by(cat) %>%
#'   mutate(n_cat = n()) %>%
#'   group_by(cat,n_cat,gov_label = gov_label != "") %>%
#'   summarise(n = n()) %>%
#'   mutate(perc = n/n_cat) %>% filter(gov_label == T)
#'
#' @export
get_accounts <- function(country,group = c("diplomats"),online = T){

  at_date = lubridate::now()
  res_cn <- NULL
  res_ru <- NULL
  res_de <- NULL
  
  if(online == T){
    base_path <- "https://raw.githubusercontent.com/schliebs/disinfo/main/inst/"
  }else if(online == F){
    base_path <- path.package("disinfo")
  }

  if("CN" %in% country & "diplomats" %in% group){

    at_date <- lubridate::today()
    accs_cn <- readr::read_csv(file.path(base_path,"extdata/cn/cn_diplomats_twitter_historical.csv"))
    #labs_cn <- readr::read_csv(file.path(base_path,"extdata/cn/platform_labels_twitter_cn_dips.csv"))
    dips_cn <- readr::read_csv(file.path(base_path,"extdata/cn/twitter_cn_dips.csv"),
                               col_types = cols(user_id = col_character()))

    out_df <-
      accs_cn %>%
      dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>%
      dplyr::select(1:9) %>% select(-user_id)
    out_df$ISO2[out_df$country == "Namibia"] <- "NA"

    res_cn <-
      out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
      #left_join(labs_cn %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
      left_join(dips_cn %>% mutate(match = str_to_lower(username )), by = "match") %>% select(-match,-date_scraped) %>%
      mutate(data_scraped = dips_cn$date_scraped[1]#,
             #labels_scraped = labs_cn$date_scraped[1]
             )

    print(paste0(nrow(res_cn)," Chinese diplomats and government accounts. Account data as of ", dips_cn$date_scraped[1],"."))

  }

  if("RU" %in% country & "diplomats" %in% group){

    at_date <- lubridate::today()
    accs_ru <- readr::read_csv(file.path(base_path,"extdata/ru/ru_diplomats_twitter_historical.csv"))
    #labs_ru <- readr::read_csv(file.path(base_path,"extdata/ru/platform_labels_twitter_ru_dips.csv"))
    dips_ru <- readr::read_csv(file.path(base_path,"extdata/ru/twitter_ru_dips.csv"),
                               col_types = cols(user_id = col_character()))

    out_df <-
      accs_ru %>%
      dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>%
      dplyr::select(1:9) %>% select(-user_id)
    out_df$ISO2[out_df$country == "Namibia"] <- "NA"

    res_ru <-
      out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
      #left_join(labs_ru %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
      left_join(dips_ru %>% mutate(match = str_to_lower(username)), by = "match") %>% select(-match,-date_scraped) %>%
      mutate(data_scraped = dips_ru$date_scraped[1]#,
             #labels_scraped = labs_ru$date_scraped[1]
             )

    print(paste0(nrow(res_ru)," Russian diplomats and government accounts. Account data as of ", dips_ru$date_scraped[1],"."))

  }
  
  if("DE" %in% country & "diplomats" %in% group){
    
    at_date <- lubridate::today()
    #accs_de <- readr::read_csv(file.path(base_path,"extdata/de/de_diplomats_twitter_historical.csv"))
    #labs_ru <- readr::read_csv(file.path(base_path,"extdata/ru/platform_labels_twitter_ru_dips.csv"))
    dips_de <- readr::read_csv(file.path(base_path,"extdata/de/twitter_de_dips.csv"),
                               col_types = cols(user_id = col_character()))
    
    accs_de <- readxl::read_xlsx("inst/extdata/de/av_accounts_v2.xlsx")
    accs_de <- accs_de %>% 
      filter(!is.na(twitter_handle)) %>% 
      mutate(ISO2 = countrycode::countrycode(country_iso,origin = "iso3c",destination = "iso2c")) %>%
      mutate(ISO2 = ifelse(country_iso == "KOS","KS",ISO2))
    
    out_df <-
      accs_de %>%
      #dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>%
      dplyr::select(1:5,ISO2) #%>% select(-user_id)
    out_df$ISO2[out_df$country == "Namibia"] <- "NA"
    
    res_de <-
      out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>%   mutate(match = str_to_lower(twitter_handle)) %>%
      #left_join(labs_ru %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
      left_join(dips_de %>% mutate(match = str_to_lower(username)), by = "match") %>% select(-match,-date_scraped) %>%
      mutate(data_scraped = dips_ru$date_scraped[1]#,
             #labels_scraped = labs_ru$date_scraped[1]
      )
    
    print(paste0(nrow(res_de)," German diplomats and government accounts. Account data as of ", dips_de$date_scraped[1],"."))
    
  }

  res <- bind_rows(res_cn,res_ru,res_de)
  return(res)

}



# get_accounts_with_labels <- function(country,group = c("diplomats"),online = T,transparency_labels = F){
#   
#   at_date = lubridate::now()
#   res_cn <- NULL
#   res_ru <- NULL
#   
#   if(online == T){
#     base_path <- "https://raw.githubusercontent.com/schliebs/disinfo/main/inst/"
#   }else if(online == F){
#     base_path <- path.package("disinfo")
#   }
#   
#   if("CN" %in% country & "diplomats" %in% group){
#     
#     at_date <- lubridate::today()
#     accs_cn <- readr::read_csv(file.path(base_path,"extdata/cn/cn_diplomats_twitter_historical.csv"))
#     labs_cn <- readr::read_csv(file.path(base_path,"extdata/cn/platform_labels_twitter_cn_dips.csv"))
#     dips_cn <- readr::read_csv(file.path(base_path,"extdata/cn/twitter_cn_dips.csv"),
#                                col_types = cols(user_id = col_character()))  
#     
#     out_df <- 
#       accs_cn %>% 
#       dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
#       dplyr::select(1:9) %>% select(-user_id)
#     out_df$ISO2[out_df$country == "Namibia"] <- "NA"
#     
#     res_cn <- 
#       out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
#       left_join(labs_cn %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
#       left_join(dips_cn %>% mutate(match = str_to_lower(screen_name)), by = "match") %>% select(-match,-date_scraped) %>% 
#       mutate(data_scraped = dips_cn$date_scraped[1],
#              labels_scraped = labs_cn$date_scraped[1])
#     
#     print(paste0(nrow(res_cn)," Chinese diplomats and government accounts. Twitter transparency labels as of ",labs_cn$date_scraped[1],". Account data as of ", dips_cn$date_scraped[1],"."))
#     
#   }
#   
#   if("RU" %in% country & "diplomats" %in% group){
#     
#     at_date <- lubridate::today()
#     accs_ru <- readr::read_csv(file.path(base_path,"extdata/ru/ru_diplomats_twitter_historical.csv"))
#     labs_ru <- readr::read_csv(file.path(base_path,"extdata/ru/platform_labels_twitter_ru_dips.csv"))
#     dips_ru <- readr::read_csv(file.path(base_path,"extdata/ru/twitter_ru_dips.csv"),
#                                col_types = cols(user_id = col_character()))  
#     
#     out_df <- 
#       accs_ru %>% 
#       dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
#       dplyr::select(1:9) %>% select(-user_id)
#     out_df$ISO2[out_df$country == "Namibia"] <- "NA"
#     
#     res_ru <- 
#       out_df %>%  filter(!is.na(twitter_handle) & twitter_handle != "") %>% select(1:4) %>%  mutate(match = str_to_lower(twitter_handle)) %>%
#       left_join(labs_ru %>% mutate(match = str_to_lower(handle)) %>% select(match,gov_label),by = "match") %>%
#       left_join(dips_ru %>% mutate(match = str_to_lower(screen_name)), by = "match") %>% select(-match,-date_scraped) %>% 
#       mutate(data_scraped = dips_ru$date_scraped[1],
#              labels_scraped = labs_ru$date_scraped[1])
#     
#     print(paste0(nrow(res_ru)," Russian diplomats and government accounts. Twitter transparency labels as of ",labs_ru$date_scraped[1],". Account data as of ", dips_ru$date_scraped[1],"."))
#     
#   }
#   
#   res <- bind_rows(res_cn,res_ru)
#   return(res)
#   
# }





