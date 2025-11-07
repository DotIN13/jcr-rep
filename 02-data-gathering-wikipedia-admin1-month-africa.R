###################
###################
## code to collect wikipedia page views data
## africa - admin 1
###################
###################


## clear environment
rm(list = ls())

## load libraries
library(wikipediatrend)
library(tidyverse)


## read in africa country list
admin1_africa <- readRDS("rds/lists/africa_dict_filled.rds")


admin1_africa$wikipedia_pt <- str_replace_all(admin1_africa$wikipedia_pt, "/", "%E2%88%95F")

years <- seq(2008, 2023, 1)
months <- seq(1, 12, 1)

basedf <- expand.grid(wikipedia_en = unique(admin1_africa$wikipedia_en),
                      year = years,
                      month = months) %>%
  left_join(admin1_africa)

## languages to consider
languages <- c("de", "en", "es", "fr", "pt", "ru", "zh")
## https://www.babbel.com/en/magazine/the-10-most-spoken-languages-in-the-world
## https://stats.wikimedia.org/EN/Sitemap.htm
## other languages to consider which do not show too much editor activity
## chinese (zh)
## hindi (hi)
## arabic (ar)
## bengali (bn)
## portuguese (pt)
## urdu (ur)

## set start and end dates for data gathering
# startdate <- "2001-01-01"
startdate <- "2008-01-01"
enddate <- "2023-12-31"




## initialize dataframe for combining data gathered for different language wikipedias
page_views_africa_admin1_all_lang <- NULL
basedf_admin1_full <- basedf

start <- Sys.time()

for (i in languages) {
  print(i)
  
  page_views_africa_admin1 <- NULL
  page_views_africa_admin1_all <- NULL
  
  for (j in 1:nrow(admin1_africa)) {
    print(admin1_africa$wikipedia_en[j])
      page_views_africa_admin1 <- wp_trend(page = ifelse(i == "ar", admin1_africa$wikipedia_ar[j],
                                                  ifelse(i == "bn", admin1_africa$wikipedia_bn[j],
                                                         ifelse(i == "de", admin1_africa$wikipedia_de[j],
                                                                ifelse(i == "en", admin1_africa$wikipedia_en[j],
                                                                       ifelse(i == "es", admin1_africa$wikipedia_es[j],
                                                                              ifelse(i == "fr", admin1_africa$wikipedia_fr[j],
                                                                                     ifelse(i == "hi", admin1_africa$wikipedia_hi[j],
                                                                                            ifelse(i == "pt", admin1_africa$wikipedia_pt[j],
                                                                                                   ifelse(i == "ru", admin1_africa$wikipedia_ru[j],
                                                                                                          ifelse(i == "ur", admin1_africa$wikipedia_ur[j], admin1_africa$wikipedia_zh[j])))))))))),
                                    from = startdate,
                                    to = enddate,
                                    lang = i,
                                    warn = T)
    print("download worked")
    page_views_africa_admin1 <- page_views_africa_admin1 %>%
      mutate(month = as.numeric(format(date, "%m")), 
             year = as.numeric(format(date, "%Y")),
             wikipedia_en = admin1_africa$wikipedia_en[j]) %>%
      select(-date, article) %>%
      group_by(wikipedia_en, month, year)%>%
      summarise(views = sum(views)) %>%
      ungroup() %>%
      ## https://stackoverflow.com/questions/62011882/usage-of-rename-function-with-paste-in-a-for-loop-in-r
      rename(!!paste0("views_", i) := views)
    print("wrangling worked")
    page_views_africa_admin1_all <- rbind(page_views_africa_admin1_all, page_views_africa_admin1)
    print("joining page views worked")
    ## bring all page views in different languages together in one dataframe
  }
  
  basedf_admin1_full <- left_join(basedf_admin1_full, page_views_africa_admin1_all#, by = c("admin1_name", "country_name", "month", "year")
  )
  print("MAINTAIN basedf_admin1_full")
  
  print("joining different language page views worked")
}

end <- Sys.time()
end-start



sum(is.na(page_views_africa_admin1_all_lang)) ## 82,564
sum(is.na(basedf_admin1_full)) ## 168,997
sapply(basedf_admin1_full, function(x) sum(is.na(x)))

basedf_admin1_full <- basedf_admin1_full %>% mutate_all(~ifelse(is.na(.x), 0, .x))

sum(is.na(basedf_admin1_full)) ## 0
sapply(basedf_admin1_full, function(x) sum(is.na(x)))

## continue here to remove unnecessary gadmname columns
saveRDS(basedf_admin1_full, "data/wikipedia/new/basedf_admin1_full.rds")
basedf_admin1_full <- readRDS("data/wikipedia/new/basedf_admin1_full.rds")

page_views_africa_admin1_all_lang_final <- basedf_admin1_full %>%
  arrange(year, month) %>%
  group_by(isocode2full, year, month) %>%
  ## create additional variables
  ## change in views from one month to another
  ## change in log views from one month to another
  summarise(
    views_en = views_en,
    views_es = views_es,
    views_fr = views_fr,
    views_pt = views_pt,
    views_de = views_de,
    views_ru = views_ru,
    views_zh = views_zh,
    views_en_log = log1p(views_en),
    views_es_log = log1p(views_es),
    views_fr_log = log1p(views_fr),
    views_pt_log = log1p(views_pt),
    views_de_log = log1p(views_de),
    views_ru_log = log1p(views_ru),
    views_zh_log = log1p(views_zh),
    views_en_change = views_en - dplyr::lag(views_en, n = 1),
    views_es_change = views_es - dplyr::lag(views_es, n = 1),
    views_fr_change = views_fr - dplyr::lag(views_fr, n = 1),
    views_pt_change = views_pt - dplyr::lag(views_pt, n = 1),
    views_de_change = views_de - dplyr::lag(views_de, n = 1),
    views_ru_change = views_ru - dplyr::lag(views_ru, n = 1),
    views_zh_change = views_zh - dplyr::lag(views_zh, n = 1),
    views_en_log_change = views_en_log - dplyr::lag(views_en_log, n = 1),
    views_es_log_change = views_es_log - dplyr::lag(views_es_log, n = 1),
    views_fr_log_change = views_fr_log - dplyr::lag(views_fr_log, n = 1), 
    views_pt_log_change = views_pt_log - dplyr::lag(views_pt_log, n = 1),
    views_de_log_change = views_de_log - dplyr::lag(views_de_log, n = 1),
    views_ru_log_change = views_ru_log - dplyr::lag(views_ru_log, n = 1),
    views_zh_log_change = views_zh_log - dplyr::lag(views_zh_log, n = 1)
  ) %>%
  # ungroup() %>%
  replace_na(list(views_en_change = 0,
                  views_es_change = 0,
                  views_fr_change = 0,
                  views_pt_change = 0,
                  views_de_change = 0,
                  views_ru_change = 0,
                  views_zh_change = 0,
                  views_en_log_change = 0,
                  views_es_log_change = 0,
                  views_fr_log_change = 0, 
                  views_pt_log_change = 0,
                  views_de_log_change = 0,
                  views_ru_log_change = 0,
                  views_zh_log_change = 0
  )
  ) %>%
  left_join(admin1_africa)

summary(page_views_africa_admin1_all_lang_final)
sum(is.na(page_views_africa_admin1_all_lang_final)) ## 0
page_views_africa_admin1_all_lang_final2 <- page_views_africa_admin1_all_lang_final %>%
  arrange(country_name, year, month)

saveRDS(page_views_africa_admin1_all_lang_final, "data/wikipedia/new/pageviews_admin1_africa_all_lang_final_20221219.rds")



