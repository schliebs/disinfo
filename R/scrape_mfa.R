library(tidyverse)
library(rvest)
library(xml2)

# also check: https://en.wikipedia.org/wiki/Ambassadors_of_China

tdy <- lubridate::today()
#tdy <- "2021-11-12"

path <- "inst/helper/prc_mission_websites/"
#path <- "ms/disinfo/inst/helper/prc_mission_websites/"

extpath <- "inst/extdata/prc_mission_websites"
#extpath <- "ms/disinfo/inst/extdata/prc_mission_websites/"



embassies <- data.frame(
  urls = c(#Embassies
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2491_665346/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2492_665348/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2493_665350/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2494_665352/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2495_665354/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2496_665356/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2490_665344/2497_665358/",
           # Consulates
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2499_665362/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2500_665364/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2501_665366/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2502_665368/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2503_665370/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2504_665372/",
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2505_665374/",
           #missions IOS etc
           "https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2506_665376/"),
  cat = c(rep("embassy",7),rep("consulate", 7),"international_organizations"),
  region = c(rep(c("Asia","Western Asia and North Africa","Africa","Europe and Central Asia",
                   "Europe","North America and Oceania","Latin America"),2),
             "global")
)


get_sub_missions <-
  function(url,cat,region){
    
    html <- 
      xml2::read_html(url,encoding = "UTF-8")
    
    urls <-
      html %>%
      html_nodes("a") 
    
    #tf <- str_detect(urls %>% html_attr('href'),"\\.\\/t")
    tf <- 
      str_detect(urls %>% html_attr('href'),"\\/t[0-9]") 
    
    # remove Houston
    nothouston <- 
      !str_detect(urls %>% html_attr('href'),"china-embassy")
    
    child_level <-
      urls %>% 
      .[tf & nothouston] %>% 
      html_attr('href') %>%
      str_remove_all("\\.\\/") 
    
    out <- paste0(url,"",child_level)
    
    child_name <- 
      urls %>% .[tf & nothouston] %>% html_text()
    
    
    return(data.frame(name = child_name,cat = cat,region = region,url = out))
    
  }

get_sub_missions(url = embassies$urls[13],
                 cat = embassies$cat[13],
                 region = embassies$region[13])


resolved_mission_links <- 
  pmap(.l = list(embassies$urls,
               embassies$cat,
               embassies$region),
    .f = get_sub_missions) %>%
  bind_rows() %>%
  mutate(id = str_extract(url, "(?<=\\/)t[0-9,_]{1,30}(?=.html)"))

#str_extract(resolved_mission_links$url[1],"(?<=\\/)t[0-9,_]{1,30}(?=.html)")


scrape_htmls <- 
  function(url,path,id){
    
    #tdy <- lubridate::today()
    
    dir.create(path = paste0(path,tdy))
    
    html <- 
      xml2::read_html(url,encoding = "UTF-8")
    
    xml2::write_html(html,
                     paste0(paste0(path,tdy),"/",id,"_",tdy,".html"))
  }


xx <- pmap(.l = list(resolved_mission_links$url,
               path,
               resolved_mission_links$id),
    .f = scrape_htmls) 



# mission_url <- list.files(paste0(path,tdy),full.names = T)[1]

get_mission_details <- 
  function(mission_url){
    
    html <- 
      xml2::read_html(mission_url,encoding = "UTF-8")
    
    content <-
      html %>%
      html_node(".content")  %>% 
      html_nodes("p") %>%
      map(.x = .,
          .f = ~ html_text(.x))
    

    # special case with different html structure: 
    # https://www.fmprc.gov.cn/mfa_eng/wjb_663304/zwjg_665342/2498_665360/2500_665364//t14546.shtml
    
    if(length(content) == 0){
      content <-
        html %>%
        html_node(".content") %>% 
        html_text2() 
      
      role <- content %>% str_extract_all(., ".+(?=:)") %>% .[[1]] %>% .[1] %>% str_trim()
      name <- content %>% str_extract_all(., "(?=:).*(?=\\\n)") %>% .[[1]] %>% .[1] %>% str_remove_all(":") %>% str_trim()
    }else{
      
      role <- content[[1]] %>% str_extract_all(., ".+(?=:)") %>% .[[1]] %>% str_trim()
      name <- content[[1]] %>% str_extract_all(., "(?<=:).*") %>% .[[1]] %>% str_trim()
      
    }
    
    if(!is.na(role) & role == "Address"){
      role <- NA
      name <- NA
    }
    
    if((!is.na(role)) & str_detect(role,"Address|Tel")){
      role <- role %>% str_extract(., ".*?(?=:)") %>% str_trim()
    }
    
    if((!is.na(name)) & str_detect(name,"Address|Tel")){
      name <- name %>% str_extract(., ".+?(?=Address|Tel)") %>% str_trim()
    }
    
    suffix <- NA
   # fullname <- name
    if((!is.na(name)) & str_detect(name,"\\(")){
      suffix <- name %>% str_extract(., "(?=\\().+(?=\\))") %>% str_remove_all("\\(|\\)")
      name <- name %>% str_extract(., ".+(?=\\()") %>% str_remove_all("\\(|\\)") %>% str_trim()
    }
    
    if((!is.na(name)) &name == "") name <- NA

    return(data.frame(role = ifelse(length(role) == 0,NA,role),
                      dip_name = ifelse(length(name) == 0,NA,name),
                      suffix = suffix,
                     # fullname,
                      mission_url))  
    
  }


#tdy <- "2021-11-18"

details <- 
  map(list.files(paste0(path,tdy),full.names = T),
      ~ get_mission_details(.x)) 

details %>%
  bind_rows() 
  
cleaned <- 
  details %>%
  bind_rows() %>% 
  mutate(id = str_extract(mission_url, "(?<=\\/)t[0-9,_]{1,30}(?=_2021-|_2022-|_2023-)")) %>% 
  left_join(resolved_mission_links,
            by = "id") %>%
  select(mission_name = name,cat,region,
         role,dip_name,suffix,
         id,url) 
  
  write_csv(cleaned,
            paste0(extpath,"/prc_missions_",tdy,".csv"))


#############

yesterday <- tdy - lubridate::days(1)
yesterday <- "2021-12-03"

df1 <- read.csv(paste0(extpath,"/prc_missions_",yesterday,".csv"))
df2 <- read.csv(paste0(extpath,"/prc_missions_",tdy,".csv"))
df3 <- read.csv(paste0(extpath,"/prc_missions_2021-11-22.csv"))

summary <- arsenal::comparedf(df1,df2, by = "id") %>% summary()
summary <- arsenal::comparedf(df3,df2, by = "id") %>% summary()
summary <- arsenal::comparedf(df3,df1, by = "id") %>% summary()

summary
summary$diffs.byvar.table$n %>% sum



# manually inspect differences

merged <-
  left_join(df3,df1,
            by = "mission_name") %>%
  select(mission_name,
         (order(colnames(.)))) %>%
  filter(dip_name.x != dip_name.y) %>% 
  set_names(colnames(.) %>% str_replace(".x","_old")%>% str_replace(".y","_new"))


###############################

if((summary$diffs.byvar.table$n %>% sum) > 0){
  
  creds <- jsonlite::read_json("ms/disinfo/inst/helper/confidential/email_credentials.json")
  
  library(emayili)
  library(magrittr)
  
  email <- envelope()
  
  email <- email %>%
    from(creds$username[[1]]) %>%
    to(creds$recipient[[1]]) 
  
  email <- email %>% subject("There was a change in PRC Diplomatic Websites")
  
  df1b <-
    df1 %>% mutate(all = paste(mission_name,cat,region,role,dip_name,suffix,id,url, 
                                sep = "; ")) %>% select(all)
  
  df2b <-
    df2 %>% mutate(all = paste(mission_name,cat,region,role,dip_name,suffix,id,url, 
                                sep = "; ")) %>% select(all)
  
  email <- email %>% 
    text(paste0("OLD: \n",
                df1b %>% filter(!all %in% df2b$all) %>% pull(all),
                "\n\n\nNEW:\n",
                df2b %>% filter(!all %in% df1b$all)%>% pull(all),
                "\n\n\n" ,collapse = "----------------------------------------------------------------------------------\n\n\n") )
  
  
  email
  
  smtp2 <- server(
    host = "smtp.gmail.com",
    port = 587,
    username = creds$username[[1]],
    password = creds$password[[1]],
    insecure = T
  )
  
  smtp2(email, verbose = TRUE)
  
}


