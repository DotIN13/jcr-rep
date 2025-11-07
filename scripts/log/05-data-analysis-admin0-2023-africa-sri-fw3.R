###################
###################
## held-out: 2023
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
# ## https://www.jottr.org/2022/12/05/avoid-detectcores/
# cores <- 48
# cl <- makeCluster(cores) ## convention to leave 1 core for OS
# registerDoParallel(cl)


ntry_bm <- seq(1, 5, by = 1)
ntry_cov <- seq(5, 40, by = 5)
ntry_gtw <- seq(5, 20, by = 5)
ntry_bm_gtw <- seq(5, 25, by = 5)
ntry_bm_cov <- seq(5, 45, by = 5)
ntry_cov_gtw <- seq(5, 60, by = 5)
ntry_bm_cov_gtw <- seq(5, 65, by = 5)
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

data_adm0_afr_2023_sri_bm_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_bm.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_bm_fw3))
sapply(data_adm0_afr_2023_sri_bm_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_bm_fw3 <- data_adm0_afr_2023_sri_bm_fw3[data_adm0_afr_2023_sri_bm_fw3$year <= 2022,]
test_adm0_afr_2023_sri_bm_fw3 <- data_adm0_afr_2023_sri_bm_fw3[data_adm0_afr_2023_sri_bm_fw3$year > 2022,]


data_adm0_afr_2023_sri_gtw_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_gtw.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_gtw_fw3))
sapply(data_adm0_afr_2023_sri_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_gtw_fw3 <- data_adm0_afr_2023_sri_gtw_fw3[data_adm0_afr_2023_sri_gtw_fw3$year <= 2022,]
test_adm0_afr_2023_sri_gtw_fw3 <- data_adm0_afr_2023_sri_gtw_fw3[data_adm0_afr_2023_sri_gtw_fw3$year > 2022,]


data_adm0_afr_2023_sri_cov_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_cov.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_cov_fw3))
sapply(data_adm0_afr_2023_sri_cov_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_cov_fw3 <- data_adm0_afr_2023_sri_cov_fw3[data_adm0_afr_2023_sri_cov_fw3$year <= 2022,]
test_adm0_afr_2023_sri_cov_fw3 <- data_adm0_afr_2023_sri_cov_fw3[data_adm0_afr_2023_sri_cov_fw3$year > 2022,]


data_adm0_afr_2023_sri_bm_gtw_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_bm_gtw.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_gtw_fw3))
sapply(data_adm0_afr_2023_sri_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_bm_gtw_fw3 <- data_adm0_afr_2023_sri_bm_gtw_fw3[data_adm0_afr_2023_sri_bm_gtw_fw3$year <= 2022,]
test_adm0_afr_2023_sri_bm_gtw_fw3 <- data_adm0_afr_2023_sri_bm_gtw_fw3[data_adm0_afr_2023_sri_bm_gtw_fw3$year > 2022,]


data_adm0_afr_2023_sri_cov_gtw_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_cov_gtw.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_cov_gtw_fw3))
sapply(data_adm0_afr_2023_sri_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_cov_gtw_fw3 <- data_adm0_afr_2023_sri_cov_gtw_fw3[data_adm0_afr_2023_sri_cov_gtw_fw3$year <= 2022,]
test_adm0_afr_2023_sri_cov_gtw_fw3 <- data_adm0_afr_2023_sri_cov_gtw_fw3[data_adm0_afr_2023_sri_cov_gtw_fw3$year > 2022,]


data_adm0_afr_2023_sri_bm_cov_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_bm_cov.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_bm_cov_fw3))
sapply(data_adm0_afr_2023_sri_bm_cov_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_bm_cov_fw3 <- data_adm0_afr_2023_sri_bm_cov_fw3[data_adm0_afr_2023_sri_bm_cov_fw3$year <= 2022,]
test_adm0_afr_2023_sri_bm_cov_fw3 <- data_adm0_afr_2023_sri_bm_cov_fw3[data_adm0_afr_2023_sri_bm_cov_fw3$year > 2022,]


data_adm0_afr_2023_sri_bm_cov_gtw_fw3 <- readRDS("rds/data/log/data_adm0_afr_sri_bm_cov_gtw.rds") %>%
  group_by(gwno) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, gwno)
sum(is.na(data_adm0_afr_2023_sri_bm_cov_gtw_fw3))
sapply(data_adm0_afr_2023_sri_bm_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm0_afr_2023_sri_bm_cov_gtw_fw3 <- data_adm0_afr_2023_sri_bm_cov_gtw_fw3[data_adm0_afr_2023_sri_bm_cov_gtw_fw3$year <= 2022,]
test_adm0_afr_2023_sri_bm_cov_gtw_fw3 <- data_adm0_afr_2023_sri_bm_cov_gtw_fw3[data_adm0_afr_2023_sri_bm_cov_gtw_fw3$year > 2022,]



ntrees <- 1000
## 5 years for training
window.length <- 36

timecontrol <- trainControl(
  method            = 'timeslice',
  initialWindow     = window.length * length(unique(data_adm0_afr_2023_sri_bm_fw3$gwno)),
  horizon           = 12*length(unique(data_adm0_afr_2023_sri_bm_fw3$gwno)),
  skip              = length(unique(data_adm0_afr_2023_sri_bm_fw3$gwno)),
  selectionFunction = "best",
  fixedWindow       = TRUE,
  # search = "random", 
  ## used to be 'final'
  savePredictions   = TRUE,
  allowParallel = TRUE
)


set.seed(0815)
rf_adm0_afr_2023_sri_bm_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                     data = train_adm0_afr_2023_sri_bm_fw3,
                                     method = "ranger",
                                     trControl = timecontrol,
                                     tuneGrid = rfGridReg_bm,
                                     num.trees = ntrees,
                                     num.threads = threads,
                                     importance = "permutation",
                                     verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_bm_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_bm_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_cov_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                      data = train_adm0_afr_2023_sri_cov_fw3,
                                      method = "ranger",
                                      trControl = timecontrol,
                                      tuneGrid = rfGridReg_cov,
                                      num.trees = ntrees,
                                      num.threads = threads,
                                      importance = "permutation",
                                      verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_cov_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_cov_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_gtw_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                      data = train_adm0_afr_2023_sri_gtw_fw3,
                                      method = "ranger",
                                      trControl = timecontrol,
                                      tuneGrid = rfGridReg_gtw,
                                      num.trees = ntrees,
                                      num.threads = threads,
                                      importance = "permutation",
                                      verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_gtw_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_gtw_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_bm_gtw_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                         data = train_adm0_afr_2023_sri_bm_gtw_fw3,
                                         method = "ranger",
                                         trControl = timecontrol,
                                         tuneGrid = rfGridReg_bm_gtw,
                                         num.trees = ntrees,
                                         num.threads = threads,
                                         importance = "permutation",
                                         verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_bm_gtw_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_bm_gtw_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_cov_gtw_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                          data = train_adm0_afr_2023_sri_cov_gtw_fw3,
                                          method = "ranger",
                                          trControl = timecontrol,
                                          tuneGrid = rfGridReg_cov_gtw,
                                          num.trees = ntrees,
                                          num.threads = threads,
                                          importance = "permutation",
                                          verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_cov_gtw_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_cov_gtw_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_bm_cov_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                         data = train_adm0_afr_2023_sri_bm_cov_fw3,
                                         method = "ranger",
                                         trControl = timecontrol,
                                         tuneGrid = rfGridReg_bm_cov,
                                         num.trees = ntrees,
                                         num.threads = threads,
                                         importance = "permutation",
                                         verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_bm_cov_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_bm_cov_fw3.rds")

set.seed(0815)
rf_adm0_afr_2023_sri_bm_cov_gtw_fw3 <- train(sri_num_log ~. - country_name - gwno - year - month - yearmonth - timeindex,
                                             data = train_adm0_afr_2023_sri_bm_cov_gtw_fw3,
                                             method = "ranger",
                                             trControl = timecontrol,
                                             tuneGrid = rfGridReg_bm_cov_gtw,
                                             num.trees = ntrees,
                                             num.threads = threads,
                                             importance = "permutation",
                                             verbose = FALSE)
saveRDS(rf_adm0_afr_2023_sri_bm_cov_gtw_fw3, "rds/ml_models/log/rf_adm0_afr_2023_sri_bm_cov_gtw_fw3.rds")


# rfbm_2023_sri_fw3 <- readRDS("rds/ml_models/log/rfbm_2023_sri_fw3.rds")
# rfcov_2023_sri_fw3 <- readRDS("rds/ml_models/log/rfcov_2023_sri_fw3.rds")
# rfgtw_2023_sri_fw3 <- readRDS("rds/ml_models/log/rfgtw_2023_sri_fw3.rds")
# rfbm_adm0_afr_2023_sri_cov_fw3 <- readRDS("rds/ml_models/log/rfbm_adm0_afr_2023_sri_cov_fw3.rds")
# rfbm_adm0_afr_2023_sri_gtw_fw3 <- readRDS("rds/ml_models/log/rfbm_adm0_afr_2023_sri_gtw_fw3.rds")
# rfcov_adm0_afr_2023_sri_gtw_fw3 <- readRDS("rds/ml_models/log/rfcov_adm0_afr_2023_sri_gtw_fw3.rds")
# rfbm_cov_adm0_afr_2023_sri_gtw_fw3 <- readRDS("rds/ml_models/log/rfbm_cov_adm0_afr_2023_sri_gtw_fw3.rds")


## create predictions
train_adm0_afr_2023_sri_bm_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_fw3, train_adm0_afr_2023_sri_bm_fw3)
test_adm0_afr_2023_sri_bm_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_fw3, test_adm0_afr_2023_sri_bm_fw3)

saveRDS(train_adm0_afr_2023_sri_bm_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_bm_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_bm_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_bm_fw3.rds")


train_adm0_afr_2023_sri_cov_fw3$pred <- predict(rf_adm0_afr_2023_sri_cov_fw3, train_adm0_afr_2023_sri_cov_fw3)
test_adm0_afr_2023_sri_cov_fw3$pred <- predict(rf_adm0_afr_2023_sri_cov_fw3, test_adm0_afr_2023_sri_cov_fw3)

saveRDS(train_adm0_afr_2023_sri_cov_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_cov_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_cov_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_cov_fw3.rds")


train_adm0_afr_2023_sri_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_gtw_fw3, train_adm0_afr_2023_sri_gtw_fw3)
test_adm0_afr_2023_sri_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_gtw_fw3, test_adm0_afr_2023_sri_gtw_fw3)

saveRDS(train_adm0_afr_2023_sri_gtw_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_gtw_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_gtw_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_gtw_fw3.rds")


train_adm0_afr_2023_sri_bm_cov_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_cov_fw3, train_adm0_afr_2023_sri_bm_cov_fw3)
test_adm0_afr_2023_sri_bm_cov_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_cov_fw3, test_adm0_afr_2023_sri_bm_cov_fw3)

saveRDS(train_adm0_afr_2023_sri_bm_cov_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_bm_cov_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_bm_cov_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_bm_cov_fw3.rds")


train_adm0_afr_2023_sri_bm_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_gtw_fw3, train_adm0_afr_2023_sri_bm_gtw_fw3)
test_adm0_afr_2023_sri_bm_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_gtw_fw3, test_adm0_afr_2023_sri_bm_gtw_fw3)

saveRDS(train_adm0_afr_2023_sri_bm_gtw_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_bm_gtw_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_bm_gtw_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_bm_gtw_fw3.rds")


train_adm0_afr_2023_sri_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_cov_gtw_fw3, train_adm0_afr_2023_sri_cov_gtw_fw3)
test_adm0_afr_2023_sri_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_cov_gtw_fw3, test_adm0_afr_2023_sri_cov_gtw_fw3)

saveRDS(train_adm0_afr_2023_sri_cov_gtw_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_cov_gtw_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_cov_gtw_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_cov_gtw_fw3.rds")


train_adm0_afr_2023_sri_bm_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_cov_gtw_fw3, train_adm0_afr_2023_sri_bm_cov_gtw_fw3)
test_adm0_afr_2023_sri_bm_cov_gtw_fw3$pred <- predict(rf_adm0_afr_2023_sri_bm_cov_gtw_fw3, test_adm0_afr_2023_sri_bm_cov_gtw_fw3)

saveRDS(train_adm0_afr_2023_sri_bm_cov_gtw_fw3, "rds/predictions/log/train_adm0_afr_2023_sri_bm_cov_gtw_fw3.rds")
saveRDS(test_adm0_afr_2023_sri_bm_cov_gtw_fw3, "rds/predictions/log/test_adm0_afr_2023_sri_bm_cov_gtw_fw3.rds")


# stopCluster(cl)
# registerDoSEQ()

