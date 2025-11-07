###################
###################
## code for preparing dataframes to be analyzed by model and outcome
## country level africa with covariate model
###################
###################

## clear environment
rm(list = ls())

library(tidyverse)

data <- readRDS("data/fulldata_adm0_africa.rds")
unique(data$country_name)
sum(is.na(data)) ## 0


## benchmark models
data_adm0_afr_sbv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sbv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_afr_sbv_bm))
saveRDS(data_adm0_afr_sbv_bm, "rds/data/log/data_adm0_afr_sbv_bm.rds")

data_adm0_afr_osv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         osv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_afr_osv_bm))
saveRDS(data_adm0_afr_osv_bm, "rds/data/log/data_adm0_afr_osv_bm.rds")

data_adm0_afr_nsv_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         nsv_fat_be_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_afr_nsv_bm))
saveRDS(data_adm0_afr_nsv_bm, "rds/data/log/data_adm0_afr_nsv_bm.rds")

data_adm0_afr_sri_bm <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sri_num_log,
         ## lagged fatalities variables
         sbv_fat_be_log_lag, osv_fat_be_log_lag, nsv_fat_be_log_lag, sri_num_log_lag, sri_fat_log_lag)
sum(is.na(data_adm0_afr_sri_bm))
saveRDS(data_adm0_afr_sri_bm, "rds/data/log/data_adm0_afr_sri_bm.rds")



## covariate models
data_adm0_afr_sbv_cov <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sbv_fat_be_log,
         ## covariates
         cinc, elev_mean, ethfrac, ethpol, farmland, forest, irregular,
         irst, milex, milper, n_leaders, nbuiltup, nethgr, newlmtnest, npetro,
         open_terrain, pec, relfrac, relpol, road_density, road_length, rugged,
         sum_igo_anytype, sum_igo_associate, sum_igo_full, sum_igo_observer,
         tpop, upop, v2x_polyarchy, wbgdp2011est, wbgdppc2011est, wbpopest, xm_qudsest,
         l1_irregular, l1_leadertransition, l1_n_leaders, l1_v2x_polyarchy,
         l1_wbgdppc2011est, l1_wbpopest, l1_xm_qudsest)
sum(is.na(data_adm0_afr_sbv_cov))
saveRDS(data_adm0_afr_sbv_cov, "rds/data/log/data_adm0_afr_sbv_cov.rds")

data_adm0_afr_osv_cov <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         osv_fat_be_log,
         ## covariates
         cinc, elev_mean, ethfrac, ethpol, farmland, forest, irregular,
         irst, milex, milper, n_leaders, nbuiltup, nethgr, newlmtnest, npetro,
         open_terrain, pec, relfrac, relpol, road_density, road_length, rugged,
         sum_igo_anytype, sum_igo_associate, sum_igo_full, sum_igo_observer,
         tpop, upop, v2x_polyarchy, wbgdp2011est, wbgdppc2011est, wbpopest, xm_qudsest,
         l1_irregular, l1_leadertransition, l1_n_leaders, l1_v2x_polyarchy,
         l1_wbgdppc2011est, l1_wbpopest, l1_xm_qudsest)
sum(is.na(data_adm0_afr_osv_cov))
saveRDS(data_adm0_afr_osv_cov, "rds/data/log/data_adm0_afr_osv_cov.rds")

data_adm0_afr_nsv_cov <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         nsv_fat_be_log,
         ## covariates
         cinc, elev_mean, ethfrac, ethpol, farmland, forest, irregular,
         irst, milex, milper, n_leaders, nbuiltup, nethgr, newlmtnest, npetro,
         open_terrain, pec, relfrac, relpol, road_density, road_length, rugged,
         sum_igo_anytype, sum_igo_associate, sum_igo_full, sum_igo_observer,
         tpop, upop, v2x_polyarchy, wbgdp2011est, wbgdppc2011est, wbpopest, xm_qudsest,
         l1_irregular, l1_leadertransition, l1_n_leaders, l1_v2x_polyarchy,
         l1_wbgdppc2011est, l1_wbpopest, l1_xm_qudsest)
sum(is.na(data_adm0_afr_nsv_cov))
saveRDS(data_adm0_afr_nsv_cov, "rds/data/log/data_adm0_afr_nsv_cov.rds")

data_adm0_afr_sri_cov <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sri_num_log,
         ## covariates
         cinc, elev_mean, ethfrac, ethpol, farmland, forest, irregular,
         irst, milex, milper, n_leaders, nbuiltup, nethgr, newlmtnest, npetro,
         open_terrain, pec, relfrac, relpol, road_density, road_length, rugged,
         sum_igo_anytype, sum_igo_associate, sum_igo_full, sum_igo_observer,
         tpop, upop, v2x_polyarchy, wbgdp2011est, wbgdppc2011est, wbpopest, xm_qudsest,
         l1_irregular, l1_leadertransition, l1_n_leaders, l1_v2x_polyarchy,
         l1_wbgdppc2011est, l1_wbpopest, l1_xm_qudsest)
sum(is.na(data_adm0_afr_sri_cov))
saveRDS(data_adm0_afr_sri_cov, "rds/data/log/data_adm0_afr_sri_cov.rds")



## google trends and wikipedia models
data_adm0_afr_sbv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sbv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sbv_fat_be_log)
sum(is.na(data_adm0_afr_sbv_gtw))
saveRDS(data_adm0_afr_sbv_gtw, "rds/data/log/data_adm0_afr_sbv_gtw.rds")

data_adm0_afr_osv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         osv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), osv_fat_be_log)
sum(is.na(data_adm0_afr_osv_gtw))
saveRDS(data_adm0_afr_osv_gtw, "rds/data/log/data_adm0_afr_osv_gtw.rds")

data_adm0_afr_nsv_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         nsv_fat_be_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), nsv_fat_be_log)
sum(is.na(data_adm0_afr_nsv_gtw))
saveRDS(data_adm0_afr_nsv_gtw, "rds/data/log/data_adm0_afr_nsv_gtw.rds")

data_adm0_afr_sri_gtw <- data %>%
  select(country_name, gwno, year, month, yearmonth,
         sri_num_log,
         ## google trends and wikipedia views data
         starts_with(c("views", "hits"))) %>%
  # select(-(ends_with(c("log", "change"))))
  select(-(matches("log$|change$")), sri_num_log)
sum(is.na(data_adm0_afr_sri_gtw))
saveRDS(data_adm0_afr_sri_gtw, "rds/data/log/data_adm0_afr_sri_gtw.rds")


## benchmark + google trends and wikipedia models
data_adm0_afr_sbv_bm_gtw <- left_join(data_adm0_afr_sbv_bm, data_adm0_afr_sbv_gtw)
sum(is.na(data_adm0_afr_sbv_bm_gtw))
saveRDS(data_adm0_afr_sbv_bm_gtw, "rds/data/log/data_adm0_afr_sbv_bm_gtw.rds")

data_adm0_afr_osv_bm_gtw <- left_join(data_adm0_afr_osv_bm, data_adm0_afr_osv_gtw)
sum(is.na(data_adm0_afr_osv_bm_gtw))
saveRDS(data_adm0_afr_osv_bm_gtw, "rds/data/log/data_adm0_afr_osv_bm_gtw.rds")

data_adm0_afr_nsv_bm_gtw <- left_join(data_adm0_afr_nsv_bm, data_adm0_afr_nsv_gtw)
sum(is.na(data_adm0_afr_nsv_bm_gtw))
saveRDS(data_adm0_afr_nsv_bm_gtw, "rds/data/log/data_adm0_afr_nsv_bm_gtw.rds")

data_adm0_afr_sri_bm_gtw <- left_join(data_adm0_afr_sri_bm, data_adm0_afr_sri_gtw)
sum(is.na(data_adm0_afr_sri_bm_gtw))
saveRDS(data_adm0_afr_sri_bm_gtw, "rds/data/log/data_adm0_afr_sri_bm_gtw.rds")


## covariate + google trends and wikipedia models
data_adm0_afr_sbv_cov_gtw <- left_join(data_adm0_afr_sbv_cov, data_adm0_afr_sbv_gtw)
sum(is.na(data_adm0_afr_sbv_cov_gtw))
saveRDS(data_adm0_afr_sbv_cov_gtw, "rds/data/log/data_adm0_afr_sbv_cov_gtw.rds")

data_adm0_afr_osv_cov_gtw <- left_join(data_adm0_afr_osv_cov, data_adm0_afr_osv_gtw)
sum(is.na(data_adm0_afr_osv_cov_gtw))
saveRDS(data_adm0_afr_osv_cov_gtw, "rds/data/log/data_adm0_afr_osv_cov_gtw.rds")

data_adm0_afr_nsv_cov_gtw <- left_join(data_adm0_afr_nsv_cov, data_adm0_afr_nsv_gtw)
sum(is.na(data_adm0_afr_nsv_cov_gtw))
saveRDS(data_adm0_afr_nsv_cov_gtw, "rds/data/log/data_adm0_afr_nsv_cov_gtw.rds")

data_adm0_afr_sri_cov_gtw <- left_join(data_adm0_afr_sri_cov, data_adm0_afr_sri_gtw)
sum(is.na(data_adm0_afr_sri_cov_gtw))
saveRDS(data_adm0_afr_sri_cov_gtw, "rds/data/log/data_adm0_afr_sri_cov_gtw.rds")


## benchmark + covariate models
data_adm0_afr_sbv_bm_cov <- left_join(data_adm0_afr_sbv_bm, data_adm0_afr_sbv_cov)
sum(is.na(data_adm0_afr_sbv_bm_cov))
saveRDS(data_adm0_afr_sbv_bm_cov, "rds/data/log/data_adm0_afr_sbv_bm_cov.rds")

data_adm0_afr_osv_bm_cov <- left_join(data_adm0_afr_osv_bm, data_adm0_afr_osv_cov)
sum(is.na(data_adm0_afr_osv_bm_cov))
saveRDS(data_adm0_afr_osv_bm_cov, "rds/data/log/data_adm0_afr_osv_bm_cov.rds")

data_adm0_afr_nsv_bm_cov <- left_join(data_adm0_afr_nsv_bm, data_adm0_afr_nsv_cov)
sum(is.na(data_adm0_afr_nsv_bm_cov))
saveRDS(data_adm0_afr_nsv_bm_cov, "rds/data/log/data_adm0_afr_nsv_bm_cov.rds")

data_adm0_afr_sri_bm_cov <- left_join(data_adm0_afr_sri_bm, data_adm0_afr_sri_cov)
sum(is.na(data_adm0_afr_sri_bm_cov))
saveRDS(data_adm0_afr_sri_bm_cov, "rds/data/log/data_adm0_afr_sri_bm_cov.rds")


## benchmark + covariate + google trends and wikipedia models
data_adm0_afr_sbv_bm_cov_gtw <- left_join(data_adm0_afr_sbv_bm_cov, data_adm0_afr_sbv_gtw)
sum(is.na(data_adm0_afr_sbv_bm_cov_gtw))
saveRDS(data_adm0_afr_sbv_bm_cov_gtw, "rds/data/log/data_adm0_afr_sbv_bm_cov_gtw.rds")

data_adm0_afr_osv_bm_cov_gtw <- left_join(data_adm0_afr_osv_bm_cov, data_adm0_afr_osv_gtw)
sum(is.na(data_adm0_afr_osv_bm_cov_gtw))
saveRDS(data_adm0_afr_osv_bm_cov_gtw, "rds/data/log/data_adm0_afr_osv_bm_cov_gtw.rds")

data_adm0_afr_nsv_bm_cov_gtw <- left_join(data_adm0_afr_nsv_bm_cov, data_adm0_afr_nsv_gtw)
sum(is.na(data_adm0_afr_nsv_bm_cov_gtw))
saveRDS(data_adm0_afr_nsv_bm_cov_gtw, "rds/data/log/data_adm0_afr_nsv_bm_cov_gtw.rds")

data_adm0_afr_sri_bm_cov_gtw <- left_join(data_adm0_afr_sri_bm_cov, data_adm0_afr_sri_gtw)
sum(is.na(data_adm0_afr_sri_bm_cov_gtw))
saveRDS(data_adm0_afr_sri_bm_cov_gtw, "rds/data/log/data_adm0_afr_sri_bm_cov_gtw.rds")
