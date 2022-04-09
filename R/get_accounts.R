library(tidyverse)

get_accounts <- function(at_date = lubridate::now()){
  
  accs <- readr::read_csv("inst/extdata/prc/prc_diplomats_twitter_historical.csv")
  accs <- readr::read_csv("inst/extdata/russia/russian_diplomats_twitter_historical.csv")
  
  out_df <- 
    accs %>% 
    dplyr::filter(is.na(ended_post) | as.Date(ended_post) > at_date) %>% 
    dplyr::select(1:9) %>% select(-user_id)
  
  #out_df[is.na(out_df)] <- ""
  
  return(out_df)
  
}

#write.csv(out_df,"inst/extdata/prc_diplomats_twitter_latest.csv")
write.csv(out_df,"inst/extdata/russia/russia_diplomats_twitter_latest.csv")
write.csv(out_df,"inst/extdata/prc/prc_diplomats_twitter_latest.csv")


######################

library(tidyverse)

df <- 
  readxl::read_excel("C:/Users/ms/Desktop/hamilton_russia.xlsx") %>% 
  mutate(handle = `User Account` %>% str_remove_all("@") %>% str_to_lower)

df3 <- 
  readxl::read_excel("C:/Users/ms/Desktop/hamilton_china.xlsx") %>% 
  mutate(handle = `User Account` %>% str_remove_all("@") %>% str_to_lower)

df2 <- 
  read_csv("inst/extdata/russia/russia_diplomats_twitter_latest.csv") %>% 
  mutate(handle = twitter_handle %>% str_to_lower)

df4 <- 
  read_csv("inst/extdata/prc/prc_diplomats_twitter_latest.csv") %>% 
  mutate(handle = twitter_handle %>% str_to_lower)

# hamilton but not my
df$handle %>% .[!. %in% df2$handle]

df3$handle %>% .[!. %in% df4$handle]

# my list but not hamilton
df2$handle %>% .[!is.na(.)] %>% .[!. %in% df$handle] 

df4$handle %>% .[!is.na(.)] %>% .[!. %in% df3$handle] 


users  <- df4$handle %>% unique() 

## get users data
usr_df <- rtweet::lookup_users(users)

users %>% .[!. %in% (usr_df$screen_name %>% str_to_lower)]

#############################
library(rtweet)
library(tidyverse)
## CNN twitter accounts
users <- df2$handle %>% unique() 

users %>% .[1:50] %>% paste0("@",.,collapse= " ")



## create CNN-accounts list with 9 total users
(ru_lst <- rtweet::post_list(users = users[201:250],   
                             list_id = "1500254439600439298",
                            # private = TRUE,
                             token = ms_token))



