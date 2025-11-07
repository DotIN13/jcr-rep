###################
###################
## code for preparing dataframes to be analyzed by model and outcome
## country level africa with covariate model
###################
###################

## clear environment
rm(list = ls())

library(tidyverse)

data <- readRDS("data/fulldata_adm1_africa.rds")
unique(data$gadmname)
sum(is.na(data)) ## 0


## benchmark models
data_adm1_afr_sbv_bm <- data %>%
  select(gadmname, isocode2full, year, month,
         sbv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm1_afr_sbv_bm))
saveRDS(data_adm1_afr_sbv_bm, "rds/data/log/data_adm1_afr_sbv_bm.rds")

data_adm1_afr_osv_bm <- data %>%
  select(gadmname, isocode2full, year, month,
         osv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm1_afr_osv_bm))
saveRDS(data_adm1_afr_osv_bm, "rds/data/log/data_adm1_afr_osv_bm.rds")

data_adm1_afr_nsv_bm <- data %>%
  select(gadmname, isocode2full, year, month,
         nsv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm1_afr_nsv_bm))
saveRDS(data_adm1_afr_nsv_bm, "rds/data/log/data_adm1_afr_nsv_bm.rds")

data_adm1_afr_sri_bm <- data %>%
  select(gadmname, isocode2full, year, month,
         sri_num_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm1_afr_sri_bm))
saveRDS(data_adm1_afr_sri_bm, "rds/data/log/data_adm1_afr_sri_bm.rds")



## covariate models
data_adm1_afr_sbv_cov <- data %>%
  select(gadmname, isocode2full, year, month,
         sbv_fat_be_log,
         ## covariates
         elev_mean, farmland, forest, gdp_ppp, nbuiltup, nethgr, nl, npetro,
         open_terrain, pop_sum, rain, road_density, road_length, temp)
sum(is.na(data_adm1_afr_sbv_cov))
saveRDS(data_adm1_afr_sbv_cov, "rds/data/log/data_adm1_afr_sbv_cov.rds")

data_adm1_afr_osv_cov <- data %>%
  select(gadmname, isocode2full, year, month,
         osv_fat_be_log,
         ## covariates
         elev_mean, farmland, forest, gdp_ppp, nbuiltup, nethgr, nl, npetro,
         open_terrain, pop_sum, rain, road_density, road_length, temp)
sum(is.na(data_adm1_afr_osv_cov))
saveRDS(data_adm1_afr_osv_cov, "rds/data/log/data_adm1_afr_osv_cov.rds")

data_adm1_afr_nsv_cov <- data %>%
  select(gadmname, isocode2full, year, month,
         nsv_fat_be_log,
         ## covariates
         elev_mean, farmland, forest, gdp_ppp, nbuiltup, nethgr, nl, npetro,
         open_terrain, pop_sum, rain, road_density, road_length, temp)
sum(is.na(data_adm1_afr_nsv_cov))
saveRDS(data_adm1_afr_nsv_cov, "rds/data/log/data_adm1_afr_nsv_cov.rds")

data_adm1_afr_sri_cov <- data %>%
  select(gadmname, isocode2full, year, month,
         sri_num_log,
         ## covariates
         elev_mean, farmland, forest, gdp_ppp, nbuiltup, nethgr, nl, npetro,
         open_terrain, pop_sum, rain, road_density, road_length, temp)
sum(is.na(data_adm1_afr_sri_cov))
saveRDS(data_adm1_afr_sri_cov, "rds/data/log/data_adm1_afr_sri_cov.rds")



## google trends and wikipedia models
data_adm1_afr_sbv_gtw <- data %>%
  select(gadmname, isocode2full, year, month,
         sbv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sbv_fat_be_log)
sum(is.na(data_adm1_afr_sbv_gtw))
saveRDS(data_adm1_afr_sbv_gtw, "rds/data/log/data_adm1_afr_sbv_gtw.rds")

data_adm1_afr_osv_gtw <- data %>%
  select(gadmname, isocode2full, year, month,
         osv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), osv_fat_be_log)
sum(is.na(data_adm1_afr_osv_gtw))
saveRDS(data_adm1_afr_osv_gtw, "rds/data/log/data_adm1_afr_osv_gtw.rds")

data_adm1_afr_nsv_gtw <- data %>%
  select(gadmname, isocode2full, year, month,
         nsv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), nsv_fat_be_log)
sum(is.na(data_adm1_afr_nsv_gtw))
saveRDS(data_adm1_afr_nsv_gtw, "rds/data/log/data_adm1_afr_nsv_gtw.rds")

data_adm1_afr_sri_gtw <- data %>%
  select(gadmname, isocode2full, year, month,
         sri_num_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sri_num_log)
sum(is.na(data_adm1_afr_sri_gtw))
saveRDS(data_adm1_afr_sri_gtw, "rds/data/log/data_adm1_afr_sri_gtw.rds")


## benchmark + google trends and wikipedia models
data_adm1_afr_sbv_bm_gtw <- left_join(data_adm1_afr_sbv_bm, data_adm1_afr_sbv_gtw)
sum(is.na(data_adm1_afr_sbv_bm_gtw))
saveRDS(data_adm1_afr_sbv_bm_gtw, "rds/data/log/data_adm1_afr_sbv_bm_gtw.rds")

data_adm1_afr_osv_bm_gtw <- left_join(data_adm1_afr_osv_bm, data_adm1_afr_osv_gtw)
sum(is.na(data_adm1_afr_osv_bm_gtw))
saveRDS(data_adm1_afr_osv_bm_gtw, "rds/data/log/data_adm1_afr_osv_bm_gtw.rds")

data_adm1_afr_nsv_bm_gtw <- left_join(data_adm1_afr_nsv_bm, data_adm1_afr_nsv_gtw)
sum(is.na(data_adm1_afr_nsv_bm_gtw))
saveRDS(data_adm1_afr_nsv_bm_gtw, "rds/data/log/data_adm1_afr_nsv_bm_gtw.rds")

data_adm1_afr_sri_bm_gtw <- left_join(data_adm1_afr_sri_bm, data_adm1_afr_sri_gtw)
sum(is.na(data_adm1_afr_sri_bm_gtw))
saveRDS(data_adm1_afr_sri_bm_gtw, "rds/data/log/data_adm1_afr_sri_bm_gtw.rds")


## covariate + google trends and wikipedia models
data_adm1_afr_sbv_cov_gtw <- left_join(data_adm1_afr_sbv_cov, data_adm1_afr_sbv_gtw)
sum(is.na(data_adm1_afr_sbv_cov_gtw))
saveRDS(data_adm1_afr_sbv_cov_gtw, "rds/data/log/data_adm1_afr_sbv_cov_gtw.rds")

data_adm1_afr_osv_cov_gtw <- left_join(data_adm1_afr_osv_cov, data_adm1_afr_osv_gtw)
sum(is.na(data_adm1_afr_osv_cov_gtw))
saveRDS(data_adm1_afr_osv_cov_gtw, "rds/data/log/data_adm1_afr_osv_cov_gtw.rds")

data_adm1_afr_nsv_cov_gtw <- left_join(data_adm1_afr_nsv_cov, data_adm1_afr_nsv_gtw)
sum(is.na(data_adm1_afr_nsv_cov_gtw))
saveRDS(data_adm1_afr_nsv_cov_gtw, "rds/data/log/data_adm1_afr_nsv_cov_gtw.rds")

data_adm1_afr_sri_cov_gtw <- left_join(data_adm1_afr_sri_cov, data_adm1_afr_sri_gtw)
sum(is.na(data_adm1_afr_sri_cov_gtw))
saveRDS(data_adm1_afr_sri_cov_gtw, "rds/data/log/data_adm1_afr_sri_cov_gtw.rds")


## benchmark + covariate models
data_adm1_afr_sbv_bm_cov <- left_join(data_adm1_afr_sbv_bm, data_adm1_afr_sbv_cov)
sum(is.na(data_adm1_afr_sbv_bm_cov))
saveRDS(data_adm1_afr_sbv_bm_cov, "rds/data/log/data_adm1_afr_sbv_bm_cov.rds")

data_adm1_afr_osv_bm_cov <- left_join(data_adm1_afr_osv_bm, data_adm1_afr_osv_cov)
sum(is.na(data_adm1_afr_osv_bm_cov))
saveRDS(data_adm1_afr_osv_bm_cov, "rds/data/log/data_adm1_afr_osv_bm_cov.rds")

data_adm1_afr_nsv_bm_cov <- left_join(data_adm1_afr_nsv_bm, data_adm1_afr_nsv_cov)
sum(is.na(data_adm1_afr_nsv_bm_cov))
saveRDS(data_adm1_afr_nsv_bm_cov, "rds/data/log/data_adm1_afr_nsv_bm_cov.rds")

data_adm1_afr_sri_bm_cov <- left_join(data_adm1_afr_sri_bm, data_adm1_afr_sri_cov)
sum(is.na(data_adm1_afr_sri_bm_cov))
saveRDS(data_adm1_afr_sri_bm_cov, "rds/data/log/data_adm1_afr_sri_bm_cov.rds")


## benchmark + covariate + google trends and wikipedia models
data_adm1_afr_sbv_bm_cov_gtw <- left_join(data_adm1_afr_sbv_bm_cov, data_adm1_afr_sbv_gtw)
sum(is.na(data_adm1_afr_sbv_bm_cov_gtw))
saveRDS(data_adm1_afr_sbv_bm_cov_gtw, "rds/data/log/data_adm1_afr_sbv_bm_cov_gtw.rds")

data_adm1_afr_osv_bm_cov_gtw <- left_join(data_adm1_afr_osv_bm_cov, data_adm1_afr_osv_gtw)
sum(is.na(data_adm1_afr_osv_bm_cov_gtw))
saveRDS(data_adm1_afr_osv_bm_cov_gtw, "rds/data/log/data_adm1_afr_osv_bm_cov_gtw.rds")

data_adm1_afr_nsv_bm_cov_gtw <- left_join(data_adm1_afr_nsv_bm_cov, data_adm1_afr_nsv_gtw)
sum(is.na(data_adm1_afr_nsv_bm_cov_gtw))
saveRDS(data_adm1_afr_nsv_bm_cov_gtw, "rds/data/log/data_adm1_afr_nsv_bm_cov_gtw.rds")

data_adm1_afr_sri_bm_cov_gtw <- left_join(data_adm1_afr_sri_bm_cov, data_adm1_afr_sri_gtw)
sum(is.na(data_adm1_afr_sri_bm_cov_gtw))
saveRDS(data_adm1_afr_sri_bm_cov_gtw, "rds/data/log/data_adm1_afr_sri_bm_cov_gtw.rds")
