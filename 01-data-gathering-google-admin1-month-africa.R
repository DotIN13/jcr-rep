###################
###################
## code to collect google trends data
## africa - admin 1
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(gtrendsR)
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
                                 retry = 999,
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
      if (grepl("== 200 is not TRUE", e)) {
      # if (grepl("== 200 ist nicht TRUE", e)) {
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



## read in africa country list
countries_africa_adm1_gtrends <- readRDS("rds/lists/africa_dict_filled.rds") %>%
  mutate(across(!isocode2full, ~ str_replace_all(., "_", " "))) %>%
  ## replace commas in names
  mutate_all(~ str_replace_all(., ", ", " ")) %>%
  mutate_all(~ str_replace_all(., "/", "%E2%88%95F")) %>%
  ## replace brackets and all info in it (e.g. "(Seychelles)")
  mutate_all(~ str_replace_all(., " \\s*\\([^\\)]+\\)", "")) %>%
  ## replace this utf encoding which wasn't caught with the utf encode function
  mutate_all(~ str_replace_all(., "%27", "'"))


## define timeframe for google searches
timeframe <- "2008-01-01 2023-12-31"
## define years and months object for null cases to loop over to attribute zero hits
years <- seq(2008, 2023, 1)
months <- seq(1, 12, 1)

basedf <- expand.grid(adm1_name = countries_africa_adm1_gtrends$wikipedia_en,
                      year = years,
                      month = months)


## categories from which to obtain googletrends data
gprops <- c("web", "news")

## languages to consider
languages <- c("de", "en", "es", "fr", "pt", "ru", "zh")

## issues with some countries / other search terms
## united arab emirates, uae
## burkina faso, burkina
## bosnia and herzegovina, bosnia, herzegovina, bosnia herzegovina
## cote d'ivoire, cote divoire, ivory coast
## democratic republic of congo, democratic republic of the congo, drc, drc congo, congo-kinshasa, congo kinshasa(, zaire)
## congo-brazzaville, congo brazzaville, republic of the congo, congo republic, republic congo
## czech republic, czechia
## united kingdom, uk
## georgia, georgia europe
## guinea-bissau, guinea bissau
## jordan, jordan middle east
## sri lanka, lanka
## myanmar, burma
## trinidad and tobago, trinidad tobago
## turkey(, turkiye, türkiye)
## united states, united states of america, us, usa
# countries_special <- data.frame(matrix(c("United Arab Emirates", c("United Arab Emirates", "UAE"),
#                                          "Burkina Faso", c("Burkina Faso", "Burkina"),
#                                          "Bosnia and Herzegovina", c("Bosnia and Herzegovina", "Bosnia Herzegovina", "Bosnia", "Herzegovina"),
#                                          "Cote D'Ivoire", c("Cote D'Ivoire", "Cote Divoire", "Ivory Coast"),
#                                          "Democratic Republic of Congo", c("")),
#                                 nrow = 2, ncol = NULL, byrow = TRUE,
#                                 dimnames = list(NULL, c("country_name", "country_name_replacement"))))


# ## languages to consider
# ## english, spanish, french, russian
# languages <- c("en", "es", "fr", "ru")
# 
# library(trendyy)
# myanmar <- trendy("Myanmar", "2019-01-01", "2020-08-13") 
# myanmar_gi <- myanmar %>% 
#   get_interest()
# myanmar_gic <- myanmar %>%
#   get_interest_country()
# ## dma: designated marketing area
# myanmar_gid <- myanmar %>%
#   get_interest_dma()
# myanmar_gir <- myanmar %>%
#   get_interest_region()
# myanmar_grq <- myanmar %>%
#   get_related_queries()
# myanmar_grt <- myanmar %>%
#   get_related_topics()
# head(myanmar)
# 
# ## look up other gtrends arguments which can be included
# code.tr <- trendy(search_terms = "Myanmar",
#                from = "2008-01-01",
#                to = "2015-12-31",
#                gprop = "web")
# 
# 
# ## gtrends frame
# ## hl = language settings: en, fr. 
# code.gt <- gtrends(keyword = "Myanmar",
#                 ## alternatively starting from 2004-01-01 when using gtrends only/separately
#                 time = "2008-01-01 2015-12-31",
#                 ## doesnt work this way, has to be one argument only
#                 ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#                 gprop = "web",
#                 hl = "en",
#                 ##continue here
#                 )
# data("categories")
# unique(categories)
# ## interesting categories:
# ## Aerospace & Defense 367
# ## Defense Industry 669
# ## others surely too, leave that for different paper
# 
# code.gt$interest_over_time
# code.gt$related_topics
# code.gt$related_queries
# 
# i <- "crisis"
# code.gt <- gtrends(keyword = paste("Myanmar", i),
#                    ## alternatively starting from 2004-01-01 when using gtrends only/separately
#                    time = "2008-01-01 2015-12-31",
#                    ## doesnt work this way, has to be one argument only
#                    ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#                    gprop = "web",
#                    hl = "en"
# )
# code.gt$interest_over_time
# code.gt <- gtrends(keyword = paste("Myanmar", i),
#                    ## alternatively starting from 2004-01-01 when using gtrends only/separately
#                    time = "2008-01-01 2015-12-31",
#                    ## doesnt work this way, has to be one argument only
#                    ## start with web (default), look into news and possibly images, youtube (,froogle?) as well
#                    gprop = "web",
#                    hl = "en"
# ) 
# 
# code.gt_ <- code.gt$interest_over_time %>%
#   mutate(month = format(date, "%m"), 
#          year = format(date, "%Y"),
#          country_name = "Myanmar") %>%
#   select(country_name, hits, month, year) %>%
#   ## https://stackoverflow.com/questions/62011882/usage-of-rename-function-with-paste-in-a-for-loop-in-r
#   if (i != "") {
#   # if (sjmisc::is_empty(i)) {
#     rename(!!paste0("hits_countryname_", i) := hits)
#   } else {
#     rename(!!paste0("hits_countryname")  := hits)
#   }
# 
# sjmisc::is_empty("")
# sjmisc::is_empty(i)

## initialize objects in which to store looping results
# googletrends_onelang <- NULL
# googletrends_onelangallgprops <- NULL
# googletrends_alllang <- NULL
googletrends_onelang <- NULL
googletrends_alllang <- basedf
googletrends_onelangallgprops <- basedf


start <- Sys.time()


for (i in languages) {
  print(i)
  
  for(j in gprops) {
    print(j)
    
    ## initialize objects to store results
    googletrends_onelangoneprop <- NULL
    
    for (k in 1:nrow(countries_africa_adm1_gtrends)) {
      googletrends_container <- NULL
      print(paste0(countries_africa_adm1_gtrends$wikipedia_en[k], ", ", j, ", ", i))

      googletrends <- gtrends_with_backoff(keyword = ifelse(i == "ar", countries_africa_adm1_gtrends$wikipedia_ar[k],
                                                            ifelse(i == "bn", countries_africa_adm1_gtrends$wikipedia_bn[k],
                                                                   ifelse(i == "de", countries_africa_adm1_gtrends$wikipedia_de[k],
                                                                          ifelse(i == "en", countries_africa_adm1_gtrends$wikipedia_en[k],
                                                                                 ifelse(i == "es", countries_africa_adm1_gtrends$wikipedia_es[k],
                                                                                        ifelse(i == "fr", countries_africa_adm1_gtrends$wikipedia_fr[k],
                                                                                               ifelse(i == "hi", countries_africa_adm1_gtrends$wikipedia_hi[k],
                                                                                                      ifelse(i == "pt", countries_africa_adm1_gtrends$wikipedia_pt[k],
                                                                                                             ifelse(i == "ru", countries_africa_adm1_gtrends$wikipedia_ru[k],
                                                                                                                    ifelse(i == "ur", countries_africa_adm1_gtrends$wikipedia_ur[k], countries_africa_adm1_gtrends$wikipedia_zh[k])))))))))),
                                           time = timeframe,
                                           low_search_volume = T,
                                           onlyInterest = T,
                                           gprop = j,
                                           hl = "en")
      print("getting trends worked")
      if (is.null(googletrends$interest_over_time)) {
        for (l in years) {
          googletrends_annual <- NULL
          for (m in months) {
            testframe <- data.frame(
              adm1_name = countries_africa_adm1_gtrends$wikipedia_en[k],
              country = countries_africa_adm1_gtrends$country[k],
              year = l,
              month = m,
              hits = 0
            ) %>%
              rename(!!paste("hits", i, j, sep = "_") := hits)
            
            googletrends_annual <- rbind(googletrends_annual, testframe)
          }
          googletrends_container <- rbind(googletrends_container, googletrends_annual)
          
        }
        
      } else {
        googletrends_ <- googletrends$interest_over_time %>%
          mutate(month = as.numeric(format(date, "%m")), 
                 year = as.numeric(format(date, "%Y")),
                 adm1_name = countries_africa_adm1_gtrends$wikipedia_en[k]) %>%
          group_by(adm1_name, month, year)%>%
          summarise(hits = mean(hits)) %>%
          ungroup() %>%
          rename(!!paste("hits", i, j, sep = "_") := hits) %>%
          mutate(country = countries_africa_adm1_gtrends$country[k]) %>%
          ## replace all NAs with 0 (eswatini on chinese and and portuguese versions)
          mutate_all(~replace(., is.na(.), 0))
        googletrends_container <- rbind(googletrends_container, googletrends_)
      }
      
      ## add isocode2full for merging with wikipedia and other data later
      if (i == languages[1]) {
        googletrends_container <- googletrends_container %>%
          mutate(gadm_name = countries_africa_adm1_gtrends$gadmname[k],
                 isocode2full = countries_africa_adm1_gtrends$isocode2full[k])
      }
        
      if (is.null(googletrends_onelangoneprop)){
        googletrends_onelangoneprop <- googletrends_container
        print("INITIALIZE onelang oneprop")
      } else {
        googletrends_onelangoneprop <- rbind(googletrends_onelangoneprop, googletrends_container)
        print("MAINTAIN onelang one prop")
      }
      
    }
    ## all countries: one language, one prop
    print(paste0("combine all countries for language ", i, " and gprop ", j))
    googletrends_onelangallgprops <- left_join(googletrends_onelangallgprops, googletrends_onelangoneprop) ##, by = c("adm1_name", "month", "year")
    print("MAINTAIN hits all")
    
    
  }
  ## container here for all languages
  ## combine results from both for loops in one object
  googletrends_alllang <- left_join(googletrends_alllang, googletrends_onelangallgprops)
  print("MAINTAIN hits all")
  
  
}

end <- Sys.time()
end-start


sum(is.na(googletrends_onelangoneprop)) ## 180 NAs
sum(is.na(googletrends_onelangallgprops)) ## 180 NAs
saveRDS(googletrends_onelangallgprops, "data/googletrends/gtrends_admin1_all_lang.rds")

