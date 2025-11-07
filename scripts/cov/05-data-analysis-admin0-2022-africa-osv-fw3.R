###################
###################
## held-out: 2022
###################
###################

# ## clear environment
# rm(list = ls())
# 
# ## look into library(caretEnsemble)
# library(dplyr)
# library(caret)
# library(ranger)
# # library(iml)
# ## for parallel computing
# library(parallel)
# library(doParallel)
# 
# # cl <- makeCluster(detectCores() - 1, setup_timeout = 0.5) ## convention to leave 1 core for OS
# ## https://www.jottr.org/2021/12/05/avoid-detectcores/
# cores <- 48
# cl <- makeCluster(cores) ## convention to leave 1 core for OS
# registerDoParallel(cl)


ntry_bm <- seq(1, 5, by = 1)
ntry_cov <- seq(4, 24, by = 4)
ntry_gtw <- seq(5, 20, by = 5)
ntry_bm_gtw <- seq(5, 25, by = 5)
ntry_bm_cov <- seq(4, 29, by = 5)
ntry_cov_gtw <- seq(4, 44, by = 5)
ntry_bm_cov_gtw <- seq(4, 49, by = 5)
minnodesize = seq(5, 10, by = 5) # default for regression is 5

## tuning parameters
## mtry - random choice of predictor variables
rfGridReg_bm <- expand.grid( mtry = ntry_bm,
                             min.node.size = minnodesize,
                             splitrule=c("extratrees","variance"))
rfGridReg_cov <- expand.grid( mtry = ntry_cov,
                              min.node.size = minnodesize,
                              splitrule=c("extratrees","variance"))
rfGridReg_gtw <- expand.grid( mtry = ntry_gtw,
                              min.node.size = minnodesize,
                              splitrule=c("extratrees","variance"))
rfGridReg_bm_gtw <- expand.grid( mtry = ntry_bm_gtw,
                                 min.node.size = minnodesize,
                                 splitrule=c("extratrees","variance"))
rfGridReg_bm_cov <- expand.grid( mtry = ntry_bm_cov,
                                 min.node.size = minnodesize,
                                 splitrule=c("extratrees","variance"))
rfGridReg_cov_gtw <- expand.grid( mtry = ntry_cov_gtw,
                                  min.node.size = minnodesize,
                                  splitrule=c("extratrees","variance"))
rfGridReg_bm_cov_gtw <- expand.grid( mtry = ntry_bm_cov_gtw,
                                     min.node.size = minnodesize,
                                     splitrule=c("extratrees","variance"))



###################
## regression
###################

data_adm0_afr_2022_osv_bm_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_bm.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_bm_fw3))
sapply(data_adm0_afr_2022_osv_bm_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_bm_fw3 <- data_adm0_afr_2022_osv_bm_fw3[data_adm0_afr_2022_osv_bm_fw3$year <= 2021,]
test_adm0_afr_2022_osv_bm_fw3 <- data_adm0_afr_2022_osv_bm_fw3[data_adm0_afr_2022_osv_bm_fw3$year > 2021,]


data_adm0_afr_2022_osv_gtw_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_gtw.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_gtw_fw3))
sapply(data_adm0_afr_2022_osv_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_gtw_fw3 <- data_adm0_afr_2022_osv_gtw_fw3[data_adm0_afr_2022_osv_gtw_fw3$year <= 2021,]
test_adm0_afr_2022_osv_gtw_fw3 <- data_adm0_afr_2022_osv_gtw_fw3[data_adm0_afr_2022_osv_gtw_fw3$year > 2021,]


data_adm0_afr_2022_osv_cov_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_cov.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_cov_fw3))
sapply(data_adm0_afr_2022_osv_cov_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_cov_fw3 <- data_adm0_afr_2022_osv_cov_fw3[data_adm0_afr_2022_osv_cov_fw3$year <= 2021,]
test_adm0_afr_2022_osv_cov_fw3 <- data_adm0_afr_2022_osv_cov_fw3[data_adm0_afr_2022_osv_cov_fw3$year > 2021,]


data_adm0_afr_2022_osv_bm_gtw_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_bm_gtw.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_gtw_fw3))
sapply(data_adm0_afr_2022_osv_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_bm_gtw_fw3 <- data_adm0_afr_2022_osv_bm_gtw_fw3[data_adm0_afr_2022_osv_bm_gtw_fw3$year <= 2021,]
test_adm0_afr_2022_osv_bm_gtw_fw3 <- data_adm0_afr_2022_osv_bm_gtw_fw3[data_adm0_afr_2022_osv_bm_gtw_fw3$year > 2021,]


data_adm0_afr_2022_osv_cov_gtw_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_cov_gtw.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_cov_gtw_fw3))
sapply(data_adm0_afr_2022_osv_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_cov_gtw_fw3 <- data_adm0_afr_2022_osv_cov_gtw_fw3[data_adm0_afr_2022_osv_cov_gtw_fw3$year <= 2021,]
test_adm0_afr_2022_osv_cov_gtw_fw3 <- data_adm0_afr_2022_osv_cov_gtw_fw3[data_adm0_afr_2022_osv_cov_gtw_fw3$year > 2021,]


data_adm0_afr_2022_osv_bm_cov_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_bm_cov.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_bm_cov_fw3))
sapply(data_adm0_afr_2022_osv_bm_cov_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_bm_cov_fw3 <- data_adm0_afr_2022_osv_bm_cov_fw3[data_adm0_afr_2022_osv_bm_cov_fw3$year <= 2021,]
test_adm0_afr_2022_osv_bm_cov_fw3 <- data_adm0_afr_2022_osv_bm_cov_fw3[data_adm0_afr_2022_osv_bm_cov_fw3$year > 2021,]


data_adm0_afr_2022_osv_bm_cov_gtw_fw3 <- readRDS("rds/data/cov/data_adm0_afr_osv_bm_cov_gtw.rds") %>%
  filter(year <= 2022) %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2022_osv_bm_cov_gtw_fw3))
sapply(data_adm0_afr_2022_osv_bm_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2022_osv_bm_cov_gtw_fw3 <- data_adm0_afr_2022_osv_bm_cov_gtw_fw3[data_adm0_afr_2022_osv_bm_cov_gtw_fw3$year <= 2021,]
test_adm0_afr_2022_osv_bm_cov_gtw_fw3 <- data_adm0_afr_2022_osv_bm_cov_gtw_fw3[data_adm0_afr_2022_osv_bm_cov_gtw_fw3$year > 2021,]



ntrees <- 1000
## 5 years for training
window.length <- 36

timecontrol <- trainControl(
  method            = 'timeslice',
  initialWindow     = window.length * length(unique(data_adm0_afr_2022_osv_bm_fw3$gwno)),
  horizon           = 12*length(unique(data_adm0_afr_2022_osv_bm_fw3$gwno)),
  skip              = length(unique(data_adm0_afr_2022_osv_bm_fw3$gwno)),
  selectionFunction = "best",
  fixedWindow       = TRUE,
  # search = "random", 
  ## used to be 'final'
  savePredictions   = TRUE,
  allowParallel = TRUE
)


# set.seed(0815)
# rf_adm0_afr_2022_osv_bm_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
#                                      data = train_adm0_afr_2022_osv_bm_fw3,
#                                      method = "ranger",
#                                      trControl = timecontrol,
#                                      tuneGrid = rfGridReg_bm,
#                                      num.trees = ntrees,
#                                      num.threads = threads,
#                                      importance = "permutation",
#                                      verbose = FALSE)
# saveRDS(rf_adm0_afr_2022_osv_bm_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_bm_fw3.rds")

set.seed(0815)
rf_adm0_afr_2022_osv_cov_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                      data = train_adm0_afr_2022_osv_cov_fw3,
                                      method = "ranger",
                                      trControl = timecontrol,
                                      tuneGrid = rfGridReg_cov,
                                      num.trees = ntrees,
                                      num.threads = threads,
                                      importance = "permutation",
                                      verbose = FALSE)
saveRDS(rf_adm0_afr_2022_osv_cov_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_cov_fw3.rds")

# set.seed(0815)
# rf_adm0_afr_2022_osv_gtw_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
#                                       data = train_adm0_afr_2022_osv_gtw_fw3,
#                                       method = "ranger",
#                                       trControl = timecontrol,
#                                       tuneGrid = rfGridReg_gtw,
#                                       num.trees = ntrees,
#                                       num.threads = threads,
#                                       importance = "permutation",
#                                       verbose = FALSE)
# saveRDS(rf_adm0_afr_2022_osv_gtw_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_gtw_fw3.rds")
# 
# set.seed(0815)
# rf_adm0_afr_2022_osv_bm_gtw_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
#                                          data = train_adm0_afr_2022_osv_bm_gtw_fw3,
#                                          method = "ranger",
#                                          trControl = timecontrol,
#                                          tuneGrid = rfGridReg_bm_gtw,
#                                          num.trees = ntrees,
#                                          num.threads = threads,
#                                          importance = "permutation",
#                                          verbose = FALSE)
# saveRDS(rf_adm0_afr_2022_osv_bm_gtw_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_bm_gtw_fw3.rds")

set.seed(0815)
rf_adm0_afr_2022_osv_cov_gtw_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                          data = train_adm0_afr_2022_osv_cov_gtw_fw3,
                                          method = "ranger",
                                          trControl = timecontrol,
                                          tuneGrid = rfGridReg_cov_gtw,
                                          num.trees = ntrees,
                                          num.threads = threads,
                                          importance = "permutation",
                                          verbose = FALSE)
saveRDS(rf_adm0_afr_2022_osv_cov_gtw_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_cov_gtw_fw3.rds")

set.seed(0815)
rf_adm0_afr_2022_osv_bm_cov_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                         data = train_adm0_afr_2022_osv_bm_cov_fw3,
                                         method = "ranger",
                                         trControl = timecontrol,
                                         tuneGrid = rfGridReg_bm_cov,
                                         num.trees = ntrees,
                                         num.threads = threads,
                                         importance = "permutation",
                                         verbose = FALSE)
saveRDS(rf_adm0_afr_2022_osv_bm_cov_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_bm_cov_fw3.rds")

set.seed(0815)
rf_adm0_afr_2022_osv_bm_cov_gtw_fw3 <- train(osv_fat_be ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                             data = train_adm0_afr_2022_osv_bm_cov_gtw_fw3,
                                             method = "ranger",
                                             trControl = timecontrol,
                                             tuneGrid = rfGridReg_bm_cov_gtw,
                                             num.trees = ntrees,
                                             num.threads = threads,
                                             importance = "permutation",
                                             verbose = FALSE)
saveRDS(rf_adm0_afr_2022_osv_bm_cov_gtw_fw3, "rds/ml_models/cov/rf_adm0_afr_2022_osv_bm_cov_gtw_fw3.rds")


# rfbm_2022_osv_fw3 <- readRDS("rds/ml_models/cov/rfbm_2022_osv_fw3.rds")
# rfcov_2022_osv_fw3 <- readRDS("rds/ml_models/cov/rfcov_2022_osv_fw3.rds")
# rfgtw_2022_osv_fw3 <- readRDS("rds/ml_models/cov/rfgtw_2022_osv_fw3.rds")
# rfbm_adm0_afr_2022_osv_cov_fw3 <- readRDS("rds/ml_models/cov/rfbm_adm0_afr_2022_osv_cov_fw3.rds")
# rfbm_adm0_afr_2022_osv_gtw_fw3 <- readRDS("rds/ml_models/cov/rfbm_adm0_afr_2022_osv_gtw_fw3.rds")
# rfcov_adm0_afr_2022_osv_gtw_fw3 <- readRDS("rds/ml_models/cov/rfcov_adm0_afr_2022_osv_gtw_fw3.rds")
# rfbm_cov_adm0_afr_2022_osv_gtw_fw3 <- readRDS("rds/ml_models/cov/rfbm_cov_adm0_afr_2022_osv_gtw_fw3.rds")


## create predictions
# train_adm0_afr_2022_osv_bm_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_fw3, train_adm0_afr_2022_osv_bm_fw3)
# test_adm0_afr_2022_osv_bm_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_fw3, test_adm0_afr_2022_osv_bm_fw3)
# 
# saveRDS(train_adm0_afr_2022_osv_bm_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_bm_fw3.rds")
# saveRDS(test_adm0_afr_2022_osv_bm_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_bm_fw3.rds")


train_adm0_afr_2022_osv_cov_fw3$pred <- predict(rf_adm0_afr_2022_osv_cov_fw3, train_adm0_afr_2022_osv_cov_fw3)
test_adm0_afr_2022_osv_cov_fw3$pred <- predict(rf_adm0_afr_2022_osv_cov_fw3, test_adm0_afr_2022_osv_cov_fw3)

saveRDS(train_adm0_afr_2022_osv_cov_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_cov_fw3.rds")
saveRDS(test_adm0_afr_2022_osv_cov_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_cov_fw3.rds")


# train_adm0_afr_2022_osv_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_gtw_fw3, train_adm0_afr_2022_osv_gtw_fw3)
# test_adm0_afr_2022_osv_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_gtw_fw3, test_adm0_afr_2022_osv_gtw_fw3)
# 
# saveRDS(train_adm0_afr_2022_osv_gtw_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_gtw_fw3.rds")
# saveRDS(test_adm0_afr_2022_osv_gtw_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_gtw_fw3.rds")


train_adm0_afr_2022_osv_bm_cov_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_cov_fw3, train_adm0_afr_2022_osv_bm_cov_fw3)
test_adm0_afr_2022_osv_bm_cov_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_cov_fw3, test_adm0_afr_2022_osv_bm_cov_fw3)

saveRDS(train_adm0_afr_2022_osv_bm_cov_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_bm_cov_fw3.rds")
saveRDS(test_adm0_afr_2022_osv_bm_cov_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_bm_cov_fw3.rds")


# train_adm0_afr_2022_osv_bm_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_gtw_fw3, train_adm0_afr_2022_osv_bm_gtw_fw3)
# test_adm0_afr_2022_osv_bm_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_gtw_fw3, test_adm0_afr_2022_osv_bm_gtw_fw3)
# 
# saveRDS(train_adm0_afr_2022_osv_bm_gtw_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_bm_gtw_fw3.rds")
# saveRDS(test_adm0_afr_2022_osv_bm_gtw_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_bm_gtw_fw3.rds")


train_adm0_afr_2022_osv_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_cov_gtw_fw3, train_adm0_afr_2022_osv_cov_gtw_fw3)
test_adm0_afr_2022_osv_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_cov_gtw_fw3, test_adm0_afr_2022_osv_cov_gtw_fw3)

saveRDS(train_adm0_afr_2022_osv_cov_gtw_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_cov_gtw_fw3.rds")
saveRDS(test_adm0_afr_2022_osv_cov_gtw_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_cov_gtw_fw3.rds")


train_adm0_afr_2022_osv_bm_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_cov_gtw_fw3, train_adm0_afr_2022_osv_bm_cov_gtw_fw3)
test_adm0_afr_2022_osv_bm_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2022_osv_bm_cov_gtw_fw3, test_adm0_afr_2022_osv_bm_cov_gtw_fw3)

saveRDS(train_adm0_afr_2022_osv_bm_cov_gtw_fw3, "rds/predictions/cov/train_adm0_afr_2022_osv_bm_cov_gtw_fw3.rds")
saveRDS(test_adm0_afr_2022_osv_bm_cov_gtw_fw3, "rds/predictions/cov/test_adm0_afr_2022_osv_bm_cov_gtw_fw3.rds")


# stopCluster(cl)
# registerDoSEQ()

