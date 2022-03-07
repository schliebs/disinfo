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


slug 

rtweet::rate_limits(token = ms_token) %>% View()

# m_schliebs
# m_schliebs = {"CONSUMER_KEY":"NUXmPRhizPbM5ql1t0Pt08hjo", 
#   "CONSUMER_SECRET":"s01Vi3KjepFddOwb5nQLIH1kus7UrocgAkPlOjKcWgRaA24sSR", 
#   "ACCESS_TOKEN":"441917082-we11QDeSbUVL1lnLIJWQhzP4GP6lI0ywiNSNDmPp", 
#   "ACCESS_TOKEN_SECRET":"rhI3QVY9TbGCwLdNe7KRD6vzRE3688HavDWrwJtQO49If"}

ms_token <-
  create_token(
   app = "mytwitterapp",
    consumer_key = "NUXmPRhizPbM5ql1t0Pt08hjo",
    consumer_secret = "s01Vi3KjepFddOwb5nQLIH1kus7UrocgAkPlOjKcWgRaA24sSR",
    access_token = "441917082-we11QDeSbUVL1lnLIJWQhzP4GP6lI0ywiNSNDmPp",
    access_secret = "rhI3QVY9TbGCwLdNe7KRD6vzRE3688HavDWrwJtQO49If",
    set_renv = TRUE
  )


ox_token <-
  create_token(
    app = "mytwitterapp",
    consumer_key = "NXy1WMLfmWuqe75hsm6pOIMNT",
    consumer_secret = "T8mybQf1bIqZBvQYsKnLiQrcURIeGvsCgJHIFb1L7aYa43xSdC",
    access_token = "1262469291821207558-5y2GBHfGUovJo7MvjPkqplcG01qs8f",
    access_secret = "zMkcpchmYpWSupKoVdYDpbF0nYHg4AWIqkbV5SoO33Hl9",
    set_renv = F
  )

rtweet::create_token()

## view list in browser
browseURL(sprintf("https://twitter.com/%s/lists/cnn-accounts",
                  rtweet:::home_user()))



############

## CNN twitter accounts
users <- c("cnn", "cnnbrk", "cnni", "cnnpolitics")

## create CNN-accounts list with 9 total users
(cnn_lst <- post_list(users,"cnn-accounts", 
                      description = "Official CNN accounts",
                      token = ox_token))



