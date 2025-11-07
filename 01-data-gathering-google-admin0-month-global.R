###################
###################
## code to collect google trends data
## global
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(gtrendsR)
library(wikipediatrend)
# library(data.table)
library(tidyverse)

## separate function which is supposed to be robust to 429 errors
## https://stackoverflow.com/questions/78098579/this-is-my-continual-error-error-in-interest-over-timewidget-comparison-item
## https://github.com/trendecon/trendecon/blob/master/R/gtrends_with_backoff.R
gtrends_with_backoff <- function(keyword = NA,
                                 geo = "",
                                 time = "today+5-y",
                                 gprop = c("web", "news", "images", "froogle", "youtube"),
                                 category = "0",
                                 hl = "en-US",
                                 low_search_volume = FALSE,
                                 cookie_url = "http://trends.google.com/Cookies/NID",
                                 tz = 0,
                                 onlyInterest = FALSE,
                                 retry = 60,
                                 wait = 5,
                                 quiet = FALSE,
                                 attempt = 1) {
  msg <- function(...) {
    if (!quiet) {
      message(...)
    }
  }
  
  if (attempt > retry) {
    stop("Retries exhausted!")
  }
  
  if (attempt == 1) {
    msg("Downloading data for ", time)
  } else {
    msg("Attempt ", attempt, "/", retry)
  }
  tryCatch(
    gtrends(
      keyword = keyword, geo = geo, time = time, gprop = gprop,
      category = category, hl = hl,
      low_search_volume = low_search_volume, cookie_url = cookie_url,
      tz = tz, onlyInterest = onlyInterest
    ),
    error = function(e) {
      # if (grepl("== 200 is not TRUE", e)) {
      if (grepl("== 200 ist nicht TRUE", e)) {
        msg("Server is not accepting requests")
      } else if (grepl("code\\:429", e)) {
        msg("Server response: 429 - too many requests")
      } else if (grepl("code\\:500", e)) {
        msg("Server response: 500 - internal server error")
      } else {
        stop(e)
      }
      
      t <- attempt * wait
      
      msg("Waiting for ", t, " seconds")
      Sys.sleep(t)
      msg("Retrying...")
      
      # Error handling by recursion
      gtrends_with_backoff(
        keyword,
        geo,
        time,
        gprop,
        category,
        hl,
        low_search_volume,
        cookie_url,
        tz,
        onlyInterest,
        retry,
        wait,
        quiet,
        attempt + 1
      )
    }
  )
}



countries_global_gtrends <- readRDS("rds/lists/countries_global_wiki.rds") %>%
  mutate_all(~ str_replace_all(., "_", " "))

## define timeframe for google searches (could go back to 2004, Wikipedia page views only start in 2008 however)
timeframe <- "2008-01-01 2023-12-31"
## define years and months object for null cases to loop over to attribute zero hits
years <- seq(2008, 2023, 1)
months <- seq(1, 12, 1)

basedf <- expand.grid(country_name = countries_global_gtrends$wikipedia_en,
                       year = years,
                       month = months)


## categories from which to obtain googletrends data
gprops <- c("web", "news")


## languages to consider
languages <- c("en", "es", "fr", "de", "pt", "ru", "zh")


# countries <- countries_global_gtrends$wikipedia_en
## initialize objects in which to store looping results
googletrends_onelang <- NULL
googletrends_alllang <- basedf
googletrends_onelangallgprops <- basedf
googletrends_container <- basedf

# for (i in keywords) {
#   print(i)
# 
#   ## initialize objects for the immediate gtrends searches and for storing results in later for loops
#   googletrends <- NULL
#   googletrends_container <- NULL
#   # googletrends_news <- NULL
#   # googletrends_news_all <- NULL
# 
#   ## too many NA/NaN arguments when looping over the combination of country name and additional keywords
#   ## for searches in news, youtube, and images, leading to loop breakdowns
#   ## solution: include news, youtube, and images only for countryname
#   ## countryname plus keywords web searches only
#   ## if keyword is empty, do web, news, youtube, and images searches
#   if (i == "") {
#     for(k in gprops) {
#       print(k)
# 
#       googletrends_container <- NULL
# 
#       for (j in countries) {
#         print(j)
#         print(i)
#         Sys.sleep(15)
#         googletrends <- gtrends(keyword = paste(j, i),
#                                 ## alternatively starting from 2004-01-01 when using gtrends only/separately
#                                 time = timeframe,
#                                 ## doesnt work this way, has to be one argument only
#                                 ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#                                 gprop = k,
#                                 ## default en-US. has only impact on related topics
#                                 ## for search results, simply searching for country names (and keywords)
#                                 ## in foreign language(s) should do the trick (future research)
#                                 hl = "en")
#         # googletrends_news <- gtrends(keyword = paste(j, i),
#         #                             ## alternatively starting from 2004-01-01 when using gtrends only/separately
#         #                             time = timeframe,
#         #                             ## doesnt work this way, has to be one argument only
#         #                             ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#         #                             gprop = "news",
#         #                             hl = "en")
#         print(i)
#         googletrends_ <- googletrends$interest_over_time %>%
#           mutate(month = format(date, "%m"),
#                  year = format(date, "%Y"),
#                  country_name = j) %>%
#           group_by(country_name, month, year)%>%
#           summarise(hits = mean(hits)) %>%
#           ungroup() %>%
#           # select(country_name, hits, month, year) %>%
#           ## https://stackoverflow.com/questions/62011882/usage-of-rename-function-with-paste-in-a-for-loop-in-r
#           # rename(ifelse(i != "", (!!paste0("hits_web_countryname_", i) := hits), (!!paste0("hits_web_countryname")  := hits)))
#           ## this works
#           # rename(!!paste0("hits_web_countryname_", i) := hits)
#           ## loop over and use paste() instead
#           ## continue here, looping over gprops
#           rename(!!paste("hits", "countryname", k, i, sep = "_") := hits)
#         # if (i != "") {
#         #   # if (sjmisc::is_empty(i)) {
#         #   rename(!!paste0("hits_web_countryname_", i) := hits)
#         # } else {
#         #   rename(!!paste0("hits_web_countryname")  := hits)
#         # }
#         print(i)
#         ## need left_join in here too, and in following loop
#         googletrends_container <- rbind(googletrends_container, googletrends_)
#       }
#       if (is.null(googletrends_allgprops)){
#         googletrends_allgprops <- googletrends_container
#         print("INITIALIZE page hits all gprops")
#       } else {
#         googletrends_allgprops <- left_join(googletrends_allgprops, googletrends_container, by = c("country_name", "month", "year"))
#         print("MAINTAIN page hits all gprops")
#       }
#     }
# 
#     ## if keyword is not empty, do only web search for combinations of countryname and keyword
#   } else {
# 
#     ## initialize objects to store results
#     googletrends_container <- NULL
#     googletrends_allweb <- NULL
# 
#     for (j in countries) {
#       print(j)
#       print(i)
#       ## times tried and tested: 0.2 (nope), 0.5 (nope), 0.8 (nope), 1 (nope), 1.5 (worked initially, later on less so)
#       ## 2, (nope),
#       Sys.sleep(15)
#       googletrends <- gtrends(keyword = paste(j, i),
#                               ## alternatively starting from 2004-01-01 when using gtrends only/separately
#                               time = timeframe,
#                               ## doesnt work this way, has to be one argument only
#                               ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#                               gprop = "web",
#                               hl = "en")
#       # googletrends_news <- gtrends(keyword = paste(j, i),
#       #                             ## alternatively starting from 2004-01-01 when using gtrends only/separately
#       #                             time = timeframe,
#       #                             ## doesnt work this way, has to be one argument only
#       #                             ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#       #                             gprop = "news",
#       #                             hl = "en")
#       print(i)
#       googletrends_ <- googletrends$interest_over_time %>%
#         mutate(month = format(date, "%m"),
#                year = format(date, "%Y"),
#                country_name = j) %>%
#         group_by(country_name, month, year)%>%
#         summarise(hits = mean(hits)) %>%
#         ungroup() %>%
#         # select(country_name, hits, month, year) %>%
#         ## https://stackoverflow.com/questions/62011882/usage-of-rename-function-with-paste-in-a-for-loop-in-r
#         # rename(ifelse(i != "", (!!paste0("hits_web_countryname_", i) := hits), (!!paste0("hits_web_countryname")  := hits)))
#         ## this works
#         rename(!!paste0("hits_countryname_web_", i) := hits)
#       ## loop over and use paste() instead
#       ## continue here, looping over gprops
#       # rename(!!paste("hits", "countryname", k, i, sep = "_") := hits)
#       # if (i != "") {
#       #   # if (sjmisc::is_empty(i)) {
#       #   rename(!!paste0("hits_web_countryname_", i) := hits)
#       # } else {
#       #   rename(!!paste0("hits_web_countryname")  := hits)
#       # }
#       # print("we got here")
#       googletrends_container <- rbind(googletrends_container, googletrends_)
#     }
# 
#     ## store allweb
#     # if (is.null(googletrends_allweb)){
#     #   googletrends_allweb <- googletrends_container
#     #   print("INITIALIZE page hits all web")
#     # } else {
#     #   googletrends_allweb <- left_join(googletrends_allweb, googletrends_container, by = c("country_name", "month", "year"))
#     #   print("MAINTAIN page hits all web")
#     # }
# 
# 
#     if (is.null(googletrends_allweb)){
#       googletrends_allweb <- googletrends_container
#       print("INITIALIZE page hits all web")
#     } else {
#       googletrends_allweb <- left_join(googletrends_allweb, googletrends_container, by = c("country_name", "month", "year"))
#       print("MAINTAIN page hits all web")
#       ## no difference for multiple times war etc
#       # googletrends_container <- NULL
#     }
# 
#   }
# 
#   print("combine all gprops and web searches")
#   # ## first bring the two above together, during each loop
#   # googletrends_allgpropsweb <- left_join(googletrends_allgprops, googletrends_allweb, by = c("country_name", "month", "year"))
#   # print("combining all gprops and web searches successful")
# 
#   ## combine results from both for loops in one object
#   if (is.null(googletrends_all)){
#     googletrends_all <- googletrends_allgprops
#     print("INITIALIZE hits all")
#   } else {
#     googletrends_all <- left_join(googletrends_all, googletrends_allweb, by = c("country_name", "month", "year"))
#     print("MAINTAIN hits all")
#   }
# 
# }


## for test purposes
## one country
## works, with some individual exceptions (es_web, es_youtube, pt_youtube)
# countries_global_gtrends <- countries_global_gtrends[1,]
## two or more countries countries
# countries_global_gtrends <- countries_global_gtrends %>%
#   filter(wikipedia_en %in% c("Algeria", "Cameroon", "Germany", "Ukraine"))


start <- Sys.time()

for (i in languages) {
  print(i)
  
  # ## initialize objects for the immediate gtrends searches and for storing results in later for loops
  # googletrends_onelangallgprops <- NULL
  # googletrends_container <- NULL
  
  for(j in gprops) {
    print(j)
    
    ## initialize objects to store results
    # googletrends_container <- NULL
    googletrends_onelangoneprop <- NULL
    
    for (k in 1:nrow(countries_global_gtrends)) {
      googletrends_container <- NULL
      print(paste0(countries_global_gtrends$wikipedia_en[k], ", ", j, ", ", i))
      ## adjusted function to catch 429 errors
      googletrends <- gtrends_with_backoff(keyword = ifelse(i == "ar", countries_global_gtrends$wikipedia_ar[k],
                                               ifelse(i == "bn", countries_global_gtrends$wikipedia_bn[k],
                                                      ifelse(i == "de", countries_global_gtrends$wikipedia_de[k],
                                                             ifelse(i == "en", countries_global_gtrends$wikipedia_en[k],
                                                                    ifelse(i == "es", countries_global_gtrends$wikipedia_es[k],
                                                                           ifelse(i == "fr", countries_global_gtrends$wikipedia_fr[k],
                                                                                  ifelse(i == "hi", countries_global_gtrends$wikipedia_hi[k],
                                                                                         ifelse(i == "pt", countries_global_gtrends$wikipedia_pt[k],
                                                                                                ifelse(i == "ru", countries_global_gtrends$wikipedia_ru[k],
                                                                                                       ifelse(i == "ur", countries_global_gtrends$wikipedia_ur[k], countries_global_gtrends$wikipedia_zh[k])))))))))),
                              time = timeframe,
                              low_search_volume = T,
                              onlyInterest = T,
                              gprop = j,
                              hl = "en")
      
      
      
      
      ## not sure the below is really necessary
      # if (countries_global_gtrends$wikipedia_en[k] == "Eswatini") {
      #   searchterm_eswatini <- url_decode_utf(ifelse((wp_linked_pages("Eswatini", "en")$lang == "fr") == TRUE, 0, wp_linked_pages("Eswatini", "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == i]))
      #   searchterm_swaziland <- url_decode_utf(ifelse((wp_linked_pages("Swaziland", "en")$lang == "fr") == TRUE, 0, wp_linked_pages("Swaziland", "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == i]))
      #   
      #   googletrends_eswatini <- gtrends_with_backoff(keyword = searchterm_eswatini,
      #                                                 ## alternatively starting from 2004-01-01 when using gtrends only/separately
      #                                                 time = timeframe,
      #                                                 ## include the low search volume clause (if it makes a difference re NAs, rerun previous runs)
      #                                                 low_search_volume = T,
      #                                                 onlyInterest = T,
      #                                                 ## doesnt work this way, has to be one argument only
      #                                                 ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
      #                                                 gprop = j,
      #                                                 ## default en-US. has only impact on related topics
      #                                                 ## for search results, simply searching for country names (and keywords)
      #                                                 ## in foreign language(s) should do the trick (future research)
      #                                                 hl = "en")
      #   googletrends_swaziland <- gtrends_with_backoff(keyword = searchterm_swaziland,
      #                                                  ## alternatively starting from 2004-01-01 when using gtrends only/separately
      #                                                  time = timeframe,
      #                                                  ## include the low search volume clause (if it makes a difference re NAs, rerun previous runs)
      #                                                  low_search_volume = T,
      #                                                  onlyInterest = T,
      #                                                  ## doesnt work this way, has to be one argument only
      #                                                  ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
      #                                                  gprop = j,
      #                                                  ## default en-US. has only impact on related topics
      #                                                  ## for search results, simply searching for country names (and keywords)
      #                                                  ## in foreign language(s) should do the trick (future research)
      #                                                  hl = "en")
      # } else {
      #   searchterm <- ifelse(i == "ar", countries_global_gtrends$wikipedia_ar[k],
      #                        ifelse(i == "bn", countries_global_gtrends$wikipedia_bn[k],
      #                               ifelse(i == "de", countries_global_gtrends$wikipedia_de[k],
      #                                      ifelse(i == "en", countries_global_gtrends$wikipedia_en[k],
      #                                             ifelse(i == "es", countries_global_gtrends$wikipedia_es[k],
      #                                                    ifelse(i == "fr", countries_global_gtrends$wikipedia_fr[k],
      #                                                           ifelse(i == "hi", countries_global_gtrends$wikipedia_hi[k],
      #                                                                  ifelse(i == "pt", countries_global_gtrends$wikipedia_pt[k],
      #                                                                         ifelse(i == "ru", countries_global_gtrends$wikipedia_ru[k],
      #                                                                                ifelse(i == "ur", countries_global_gtrends$wikipedia_ur[k], countries_global_gtrends$wikipedia_zh[k]))))))))))
      #   googletrends <- gtrends_with_backoff(keyword = searchterm,
      #                                        ## alternatively starting from 2004-01-01 when using gtrends only/separately
      #                                        time = timeframe,
      #                                        ## include the low search volume clause (if it makes a difference re NAs, rerun previous runs)
      #                                        low_search_volume = T,
      #                                        onlyInterest = T,
      #                                        ## doesnt work this way, has to be one argument only
      #                                        ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
      #                                        gprop = j,
      #                                        ## default en-US. has only impact on related topics
      #                                        ## for search results, simply searching for country names (and keywords)
      #                                        ## in foreign language(s) should do the trick (future research)
      #                                        hl = "en")
      #   
      # }
      
      print("getting trends worked")
      if (is.null(googletrends$interest_over_time)) {
        for (l in years) {
          googletrends_annual <- NULL
          for (m in months) {
            # testframe <- NULL
            testframe <- data.frame(
              country_name = countries_global_gtrends$wikipedia_en[k],
              year = l,
              month = m,
              hits = 0
            ) %>%
              rename(!!paste("hits", i, j, sep = "_") := hits)
            
            googletrends_annual <- rbind(googletrends_annual, testframe)
          }
          ## test code
          # googletrends_all <- rbind(googletrends_all, googletrends_annual)
          googletrends_container <- rbind(googletrends_container, googletrends_annual)
          
        }
        
      } else {
        googletrends_ <- googletrends$interest_over_time %>%
          mutate(month = as.numeric(format(date, "%m")), 
                 year = as.numeric(format(date, "%Y")),
                 country_name = countries_global_gtrends$wikipedia_en[k]) %>%
          group_by(country_name, month, year)%>%
          summarise(hits = mean(hits)) %>%
          ungroup() %>%
          rename(!!paste("hits", i, j, sep = "_") := hits) %>%
          mutate_all(~replace(., is.na(.), 0))
        googletrends_container <- rbind(googletrends_container, googletrends_)
      }
      
      
      if (is.null(googletrends_onelangoneprop)){
        googletrends_onelangoneprop <- googletrends_container
        print("INITIALIZE onelang oneprop")
      } else {
        googletrends_onelangoneprop <- rbind(googletrends_onelangoneprop, googletrends_container)
        print("MAINTAIN onelang one prop")
      }
      
    }
    print(paste0("combine all countries for language ", i,  " and gprop ", j))
    # ## first bring the two above together, during each loop
    # googletrends_allgpropsweb <- left_join(googletrends_allgprops, googletrends_allweb, by = c("country_name", "month", "year"))
    # print("combining all gprops and web searches successful")
    
    ## combine results from both for loops in one object
    ## container here for all gprops for one language
    # if (is.null(googletrends_onelangallgprops)){
    #   googletrends_onelangallgprops <- googletrends_onelangoneprop
    #   print("INITIALIZE hits all")
    # } else {
    #   googletrends_onelangallgprops <- left_join(googletrends_onelangallgprops, googletrends_onelangoneprop, by = c("country_name", "month", "year"))
    #   # googletrends_onelangallgprops <- rbind(googletrends_onelangallgprops, googletrends_onelangoneprop)
    #   print("MAINTAIN hits all")
    # }
    googletrends_onelangallgprops <- left_join(googletrends_onelangallgprops, googletrends_onelangoneprop, by = c("country_name", "month", "year"))
    # googletrends_onelangallgprops <- rbind(googletrends_onelangallgprops, googletrends_onelangoneprop)
    print("MAINTAIN hits all")
    
    
  }
  googletrends_alllang <- left_join(googletrends_alllang, googletrends_onelangallgprops)
  print("MAINTAIN hits all")
  
  
}

end <- Sys.time()
end-start

sum(is.na(googletrends_onelangoneprop))
sum(is.null(googletrends_onelangoneprop))

sum(is.na(googletrends_onelangallgprops))
sum(is.null(googletrends_onelangallgprops))

## eswatini and swaziland currently separate
eswatini <- googletrends_onelangallgprops %>%
  filter(country_name == "Eswatini")
swazi <- googletrends_onelangallgprops %>%
  filter(country_name == "Swaziland")

googletrends_onelangallgprops$country_name[googletrends_onelangallgprops$country_name == "Swaziland"] <- "Eswatini"
eswatini2 <- googletrends_onelangallgprops %>%
  filter(country_name == "Eswatini")
googletrends_onelangallgprops <- googletrends_onelangallgprops %>%
  group_by(country_name, year, month) %>%
  summarise(hits_en_web = mean(hits_en_web),
            hits_en_news = mean(hits_en_news),
            hits_es_web = mean(hits_es_web),
            hits_es_news = mean(hits_es_news),
            hits_fr_web = mean(hits_fr_web),
            hits_fr_news = mean(hits_fr_news),
            hits_de_web = mean(hits_de_web),
            hits_de_news = mean(hits_de_news),
            hits_pt_web = mean(hits_pt_web),
            hits_pt_news = mean(hits_pt_news),
            hits_ru_web = mean(hits_ru_web),
            hits_ru_news = mean(hits_ru_news),
            hits_zh_web = mean(hits_zh_web),
            hits_zh_news = mean(hits_zh_news)
            ) %>%
  ungroup()


saveRDS(googletrends_onelangallgprops, "data/googletrends/gtrends_country_global_all_lang_all_grops.rds")

