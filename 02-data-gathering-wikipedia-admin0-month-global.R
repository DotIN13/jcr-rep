###################
###################
## code to collect wikipedia page views data
## global
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(wikipediatrend)
library(tidyverse)

## read in global country list
countries_global <- readRDS("rds/lists/countries_global_wiki.rds")


## languages to consider
## english, spanish, french, russian (leave russian out for now)
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
startdate <- "2008-01-01"
enddate <- "2023-12-31"


## initialize dataframe for combining data gathered for different language wikipedias
page_views_global_all_lang <- NULL

for (i in languages) {
  print(i)
  
  page_views_global <- NULL
  page_views_global_all <- NULL
  
  for (j in 1:nrow(countries_global)) {
    print(countries_global$wikipedia_en[j])
    ## previously Swaziland
    if (countries_global$wikipedia_en[j] == "Eswatini") {
      page_views_global <- wp_trend(page = ifelse(i == "ar", c("سوازيلاند", countries_global$wikipedia_ar[j]),
                                                  ifelse(i == "bn", c("সোয়াজিল্যান্ড", countries_global$wikipedia_bn[j]),
                                                         ifelse(i == "de", c("Swasiland", countries_global$wikipedia_de[j]),
                                                                ifelse(i == "en", c("Swaziland", countries_global$wikipedia_en[j]),
                                                                       ifelse(i == "es", c("Suazilandia", countries_global$wikipedia_es[j]),
                                                                              ifelse(i == "fr", c("Swaziland", countries_global$wikipedia_fr[j]),
                                                                                     ifelse(i == "hi", c("स्वाजीलैंड", countries_global$wikipedia_hi[j]),
                                                                                            ifelse(i == "pt", c("Suazilândia", countries_global$wikipedia_pt[j]),
                                                                                                   ifelse(i == "ru", c("Свазиленд", countries_global$wikipedia_ru[j]),
                                                                                                          ## chinese: first simplified, then traditional
                                                                                                          ifelse(i == "ur", c("سوازی لینڈ", countries_global$wikipedia_ur[j]), c("斯威士兰", "<斯威士蘭", countries_global$wikipedia_zh[j]))))))))))),
                                    from = startdate,
                                    to = enddate,
                                    lang = i,
                                    warn = T)
    ## previously Macedonia; google translated Macedonia
    } else if (countries_global$wikipedia_en[j] == "North_Macedonia") {
      page_views_global <- wp_trend(page = ifelse(i == "ar", c("مقدونيا", countries_global$wikipedia_ar[j]),
                                                  ifelse(i == "bn", c("মেসিডোনিয়া", countries_global$wikipedia_bn[j]),
                                                         ifelse(i == "de", c("Mazedonien", countries_global$wikipedia_de[j]),
                                                                ifelse(i == "en", c("Macedonia", countries_global$wikipedia_en[j]),
                                                                       ifelse(i == "es", c("Macedonia", countries_global$wikipedia_es[j]),
                                                                              ifelse(i == "fr", c("Macédoine", countries_global$wikipedia_fr[j]),
                                                                                     ifelse(i == "hi", c("मैसेडोनिया", countries_global$wikipedia_hi[j]),
                                                                                            ifelse(i == "pt", c("Macedônia", "Macedônio", countries_global$wikipedia_pt[j]),
                                                                                                   ifelse(i == "ru", c("Македония", countries_global$wikipedia_ru[j]),
                                                                                                          ## chinese: first simplified, then traditional
                                                                                                          ifelse(i == "ur", c("مقدونیہ", countries_global$wikipedia_ur[j]), c("马其顿", "馬其頓", countries_global$wikipedia_zh[j]))))))))))),
                                    from = startdate,
                                    to = enddate,
                                    lang = i,
                                    warn = T)
      
    } else {
      page_views_global <- wp_trend(page = ifelse(i == "ar", countries_global$wikipedia_ar[j],
                                                  ifelse(i == "bn", countries_global$wikipedia_bn[j],
                                                         ifelse(i == "de", countries_global$wikipedia_de[j],
                                                                ifelse(i == "en", countries_global$wikipedia_en[j],
                                                                       ifelse(i == "es", countries_global$wikipedia_es[j],
                                                                              ifelse(i == "fr", countries_global$wikipedia_fr[j],
                                                                                     ifelse(i == "hi", countries_global$wikipedia_hi[j],
                                                                                            ifelse(i == "pt", countries_global$wikipedia_pt[j],
                                                                                                   ifelse(i == "ru", countries_global$wikipedia_ru[j],
                                                                                                          ifelse(i == "ur", countries_global$wikipedia_ur[j], countries_global$wikipedia_zh[j])))))))))),
                                    from = startdate,
                                    to = enddate,
                                    lang = i,
                                    warn = T)
    }
    print("download worked")
    page_views_global <- page_views_global %>%
      mutate(month = format(date, "%m"), 
             year = format(date, "%Y"),
             country_name = countries_global$wikipedia_en[j]) %>%
      select(-date, article) %>%
      group_by(country_name, month, year)%>%
      summarise(views = sum(views)) %>%
      ungroup() %>%
      ## https://stackoverflow.com/questions/62011882/usage-of-rename-function-with-paste-in-a-for-loop-in-r
      rename(!!paste0("views_", i) := views)
    print("wrangling worked")
    page_views_global_all <- rbind(page_views_global_all, page_views_global)
    print("joining page views worked")
    ## bring all page views in different languages together in one dataframe
  }
  if (is.null(page_views_global_all_lang)){
    page_views_global_all_lang <- page_views_global_all
    print("INITIALIZE page views all lang")
  } else {
    page_views_global_all_lang <- left_join(page_views_global_all_lang, page_views_global_all, by = c("country_name", "month", "year"))
    print("MAINTAIN page views all lang")
  }
  print("joining different language page views worked")
}

## eswatini and swaziland currently separate
eswatini <- page_views_global_all_lang %>%
  filter(country_name == "Eswatini")
swazi <- page_views_global_all_lang %>%
  filter(country_name == "Swaziland")

page_views_global_all_lang$country_name[page_views_global_all_lang$country_name == "Swaziland"] <- "Eswatini"
eswatini2 <- page_views_global_all_lang %>%
  filter(country_name == "Eswatini")
page_views_global_all_lang <- page_views_global_all_lang %>%
  group_by(country_name, year, month) %>%
  summarise(views_en = sum(views_en),
            views_es = sum(views_es),
            views_fr = sum(views_fr),
            views_de = sum(views_de),
            views_pt = sum(views_pt),
            views_ru = sum(views_ru),
            views_zh = sum(views_zh)) %>%
  ungroup()

dim(page_views_global_all_lang) ## 37,248 (january 2008 through december 2023)
sum(is.na(page_views_global_all_lang))
sapply(page_views_global_all_lang, function(x) sum(is.na(x)))

missings_tab <- page_views_global_all_lang[rowSums(is.na(page_views_global_all_lang)) > 0, ]

missings_tab %>%
  naniar::gg_miss_upset()
Amelia::missmap(missings_tab)
ls(missings_tab)

unique(missings_tab$country_name)

sapply(page_views_global_all_lang, function(x) sum(is.na(x)))
apply(is.na(page_views_global_all_lang),2,sum)
colSums(is.na(page_views_global_all_lang))




saveRDS(page_views_global_all_lang, "data/wikipedia/new/pageviews_global_country_all_lang.rds")

## unique() probably not necessary anymore
page_views_global_all_lang <- readRDS("data/wikipedia/new/pageviews_global_country_all_lang.rds") %>% 
  ## replace all NAs with 0 (eswatini on chinese and and portuguese versions)
  mutate_all(~replace(., is.na(.), 0))



page_views_global_all_lang_final <- page_views_global_all_lang %>%
  ## create log views variables
  mutate(views_en_log = log1p(views_en),
         views_es_log = log1p(views_es),
         views_fr_log = log1p(views_fr),
         views_pt_log = log1p(views_pt),
         views_de_log = log1p(views_de),
         views_ru_log = log1p(views_ru),
         views_zh_log = log1p(views_zh)) %>%
  arrange(year, month) %>%
  group_by(country_name) %>%
  ## create additional variables
  ## change in views from one month to another
  ## change in log views from one month to another
  mutate(
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
  ungroup() %>%
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
  )

summary(page_views_global_all_lang_final)
sum(is.na(page_views_global_all_lang_final)) ## 0
page_views_global_all_lang_final2 <- page_views_global_all_lang_final %>%
  arrange(country_name, year, month)

saveRDS(page_views_global_all_lang_final, "data/wikipedia/new/pageviews_country_global_all_lang_final_20240316.rds")

