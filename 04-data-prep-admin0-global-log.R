###################
###################
## code for preparing dataframes to be analyzed by model and outcome
## country level global
###################
###################

## clear environment
rm(list = ls())

library(tidyverse)

data <- readRDS("data/fulldata_global.rds")
unique(data$country_name)
sum(is.na(data))

data_adm0_glob_sbv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sbv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_glob_sbv_bm))
saveRDS(data_adm0_glob_sbv_bm, "rds/data/log/data_adm0_glob_sbv_bm.rds")


## benchmark models
data_adm0_glob_osv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         osv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_glob_osv_bm))
saveRDS(data_adm0_glob_osv_bm, "rds/data/log/data_adm0_glob_osv_bm.rds")

data_adm0_glob_nsv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         nsv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_glob_nsv_bm))
saveRDS(data_adm0_glob_nsv_bm, "rds/data/log/data_adm0_glob_nsv_bm.rds")

data_adm0_glob_sri_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sri_num_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_glob_sri_bm))
saveRDS(data_adm0_glob_sri_bm, "rds/data/log/data_adm0_glob_sri_bm.rds")


## google trends and wikipedia models
data_adm0_glob_sbv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sbv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sbv_fat_be_log)
sum(is.na(data_adm0_glob_sbv_gtw))
saveRDS(data_adm0_glob_sbv_gtw, "rds/data/log/data_adm0_glob_sbv_gtw.rds")

data_adm0_glob_osv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         osv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), osv_fat_be_log)
sum(is.na(data_adm0_glob_osv_gtw))
saveRDS(data_adm0_glob_osv_gtw, "rds/data/log/data_adm0_glob_osv_gtw.rds")

data_adm0_glob_nsv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         nsv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), nsv_fat_be_log)
sum(is.na(data_adm0_glob_nsv_gtw))
saveRDS(data_adm0_glob_nsv_gtw, "rds/data/log/data_adm0_glob_nsv_gtw.rds")

data_adm0_glob_sri_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sri_num_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sri_num_log)
sum(is.na(data_adm0_glob_sri_gtw))
saveRDS(data_adm0_glob_sri_gtw, "rds/data/log/data_adm0_glob_sri_gtw.rds")

## benchmark + google trends and wikipedia models
data_adm0_glob_sbv_bm_gtw <- left_join(data_adm0_glob_sbv_bm, data_adm0_glob_sbv_gtw)
sum(is.na(data_adm0_glob_sbv_bm_gtw))
saveRDS(data_adm0_glob_sbv_bm_gtw, "rds/data/log/data_adm0_glob_sbv_bm_gtw.rds")

data_adm0_glob_osv_bm_gtw <- left_join(data_adm0_glob_osv_bm, data_adm0_glob_osv_gtw)
sum(is.na(data_adm0_glob_osv_bm_gtw))
saveRDS(data_adm0_glob_osv_bm_gtw, "rds/data/log/data_adm0_glob_osv_bm_gtw.rds")

data_adm0_glob_nsv_bm_gtw <- left_join(data_adm0_glob_nsv_bm, data_adm0_glob_nsv_gtw)
sum(is.na(data_adm0_glob_nsv_bm_gtw))
saveRDS(data_adm0_glob_nsv_bm_gtw, "rds/data/log/data_adm0_glob_nsv_bm_gtw.rds")

data_adm0_glob_sri_bm_gtw <- left_join(data_adm0_glob_sri_bm, data_adm0_glob_sri_gtw)
sum(is.na(data_adm0_glob_sri_bm_gtw))
saveRDS(data_adm0_glob_sri_bm_gtw, "rds/data/log/data_adm0_glob_sri_bm_gtw.rds")
