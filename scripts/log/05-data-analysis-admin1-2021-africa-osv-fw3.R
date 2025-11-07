###################
###################
## held-out: 2021
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
# ## https://www.jottr.org/2020/12/05/avoid-detectcores/
# cores <- 48
# cl <- makeCluster(cores) ## convention to leave 1 core for OS
# registerDoParallel(cl)


ntry_bm <- seq(1, 5, by = 1)
ntry_cov <- seq(2, 14, by = 2)
ntry_gtw <- seq(5, 20, by = 5)
ntry_bm_gtw <- seq(5, 25, by = 5)
ntry_bm_cov <- seq(5, 20, by = 5)
ntry_cov_gtw <- seq(5, 35, by = 5)
ntry_bm_cov_gtw <- seq(5, 40, by = 5)
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

data_adm1_afr_2021_osv_bm_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_bm.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_bm_fw3))
sapply(data_adm1_afr_2021_osv_bm_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_bm_fw3 <- data_adm1_afr_2021_osv_bm_fw3[data_adm1_afr_2021_osv_bm_fw3$year <= 2020,]
test_adm1_afr_2021_osv_bm_fw3 <- data_adm1_afr_2021_osv_bm_fw3[data_adm1_afr_2021_osv_bm_fw3$year > 2020,]


data_adm1_afr_2021_osv_gtw_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_gtw.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_gtw_fw3))
sapply(data_adm1_afr_2021_osv_gtw_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_gtw_fw3 <- data_adm1_afr_2021_osv_gtw_fw3[data_adm1_afr_2021_osv_gtw_fw3$year <= 2020,]
test_adm1_afr_2021_osv_gtw_fw3 <- data_adm1_afr_2021_osv_gtw_fw3[data_adm1_afr_2021_osv_gtw_fw3$year > 2020,]


data_adm1_afr_2021_osv_cov_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_cov.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_cov_fw3))
sapply(data_adm1_afr_2021_osv_cov_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_cov_fw3 <- data_adm1_afr_2021_osv_cov_fw3[data_adm1_afr_2021_osv_cov_fw3$year <= 2020,]
test_adm1_afr_2021_osv_cov_fw3 <- data_adm1_afr_2021_osv_cov_fw3[data_adm1_afr_2021_osv_cov_fw3$year > 2020,]


data_adm1_afr_2021_osv_bm_gtw_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_bm_gtw.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_bm_gtw_fw3))
sapply(data_adm1_afr_2021_osv_bm_gtw_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_bm_gtw_fw3 <- data_adm1_afr_2021_osv_bm_gtw_fw3[data_adm1_afr_2021_osv_bm_gtw_fw3$year <= 2020,]
test_adm1_afr_2021_osv_bm_gtw_fw3 <- data_adm1_afr_2021_osv_bm_gtw_fw3[data_adm1_afr_2021_osv_bm_gtw_fw3$year > 2020,]


data_adm1_afr_2021_osv_cov_gtw_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_cov_gtw.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_cov_gtw_fw3))
sapply(data_adm1_afr_2021_osv_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_cov_gtw_fw3 <- data_adm1_afr_2021_osv_cov_gtw_fw3[data_adm1_afr_2021_osv_cov_gtw_fw3$year <= 2020,]
test_adm1_afr_2021_osv_cov_gtw_fw3 <- data_adm1_afr_2021_osv_cov_gtw_fw3[data_adm1_afr_2021_osv_cov_gtw_fw3$year > 2020,]


data_adm1_afr_2021_osv_bm_cov_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_bm_cov.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_bm_cov_fw3))
sapply(data_adm1_afr_2021_osv_bm_cov_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_bm_cov_fw3 <- data_adm1_afr_2021_osv_bm_cov_fw3[data_adm1_afr_2021_osv_bm_cov_fw3$year <= 2020,]
test_adm1_afr_2021_osv_bm_cov_fw3 <- data_adm1_afr_2021_osv_bm_cov_fw3[data_adm1_afr_2021_osv_bm_cov_fw3$year > 2020,]


data_adm1_afr_2021_osv_bm_cov_gtw_fw3 <- readRDS("rds/data/log/data_adm1_afr_osv_bm_cov_gtw.rds") %>%
  filter(year <= 2021) %>%
  group_by(isocode2full) %>%
  mutate(timeindex = row_number()) %>%
  ungroup() %>%
  arrange(timeindex, isocode2full)
sum(is.na(data_adm1_afr_2021_osv_bm_cov_gtw_fw3))
sapply(data_adm1_afr_2021_osv_bm_cov_gtw_fw3, function(x) sum(is.na(x)))

train_adm1_afr_2021_osv_bm_cov_gtw_fw3 <- data_adm1_afr_2021_osv_bm_cov_gtw_fw3[data_adm1_afr_2021_osv_bm_cov_gtw_fw3$year <= 2020,]
test_adm1_afr_2021_osv_bm_cov_gtw_fw3 <- data_adm1_afr_2021_osv_bm_cov_gtw_fw3[data_adm1_afr_2021_osv_bm_cov_gtw_fw3$year > 2020,]



ntrees <- 1000
## 5 years for training
window.length <- 36

timecontrol <- trainControl(
  method            = 'timeslice',
  initialWindow     = window.length * length(unique(data_adm1_afr_2021_osv_bm_fw3$isocode2full)),
  horizon           = 12*length(unique(data_adm1_afr_2021_osv_bm_fw3$isocode2full)),
  skip              = length(unique(data_adm1_afr_2021_osv_bm_fw3$isocode2full)),
  selectionFunction = "best",
  fixedWindow       = TRUE,
  # search = "random", 
  ## used to be 'final'
  savePredictions   = TRUE,
  allowParallel = TRUE
)


set.seed(0815)
rf_adm1_afr_2021_osv_bm_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                     data = train_adm1_afr_2021_osv_bm_fw3,
                                     method = "ranger",
                                     trControl = timecontrol,
                                     tuneGrid = rfGridReg_bm,
                                     num.trees = ntrees,
                                     num.threads = threads,
                                     importance = "permutation",
                                     verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_bm_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_bm_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_cov_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                      data = train_adm1_afr_2021_osv_cov_fw3,
                                      method = "ranger",
                                      trControl = timecontrol,
                                      tuneGrid = rfGridReg_cov,
                                      num.trees = ntrees,
                                      num.threads = threads,
                                      importance = "permutation",
                                      verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_cov_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_cov_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_gtw_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                      data = train_adm1_afr_2021_osv_gtw_fw3,
                                      method = "ranger",
                                      trControl = timecontrol,
                                      tuneGrid = rfGridReg_gtw,
                                      num.trees = ntrees,
                                      num.threads = threads,
                                      importance = "permutation",
                                      verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_gtw_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_gtw_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_bm_gtw_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                         data = train_adm1_afr_2021_osv_bm_gtw_fw3,
                                         method = "ranger",
                                         trControl = timecontrol,
                                         tuneGrid = rfGridReg_bm_gtw,
                                         num.trees = ntrees,
                                         num.threads = threads,
                                         importance = "permutation",
                                         verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_bm_gtw_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_bm_gtw_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_cov_gtw_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                          data = train_adm1_afr_2021_osv_cov_gtw_fw3,
                                          method = "ranger",
                                          trControl = timecontrol,
                                          tuneGrid = rfGridReg_cov_gtw,
                                          num.trees = ntrees,
                                          num.threads = threads,
                                          importance = "permutation",
                                          verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_cov_gtw_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_cov_gtw_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_bm_cov_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                         data = train_adm1_afr_2021_osv_bm_cov_fw3,
                                         method = "ranger",
                                         trControl = timecontrol,
                                         tuneGrid = rfGridReg_bm_cov,
                                         num.trees = ntrees,
                                         num.threads = threads,
                                         importance = "permutation",
                                         verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_bm_cov_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_bm_cov_fw3.rds")

set.seed(0815)
rf_adm1_afr_2021_osv_bm_cov_gtw_fw3 <- train(osv_fat_be_log ~. - gadmname - isocode2full - year - month - timeindex,
                                             data = train_adm1_afr_2021_osv_bm_cov_gtw_fw3,
                                             method = "ranger",
                                             trControl = timecontrol,
                                             tuneGrid = rfGridReg_bm_cov_gtw,
                                             num.trees = ntrees,
                                             num.threads = threads,
                                             importance = "permutation",
                                             verbose = FALSE)
saveRDS(rf_adm1_afr_2021_osv_bm_cov_gtw_fw3, "rds/ml_models/log/rf_adm1_afr_2021_osv_bm_cov_gtw_fw3.rds")


# rfbm_2021_osv_fw3 <- readRDS("rds/ml_models/log/rfbm_2021_osv_fw3.rds")
# rfcov_2021_osv_fw3 <- readRDS("rds/ml_models/log/rfcov_2021_osv_fw3.rds")
# rfgtw_2021_osv_fw3 <- readRDS("rds/ml_models/log/rfgtw_2021_osv_fw3.rds")
# rfbm_adm1_afr_2021_osv_cov_fw3 <- readRDS("rds/ml_models/log/rfbm_adm1_afr_2021_osv_cov_fw3.rds")
# rfbm_adm1_afr_2021_osv_gtw_fw3 <- readRDS("rds/ml_models/log/rfbm_adm1_afr_2021_osv_gtw_fw3.rds")
# rfcov_adm1_afr_2021_osv_gtw_fw3 <- readRDS("rds/ml_models/log/rfcov_adm1_afr_2021_osv_gtw_fw3.rds")
# rfbm_cov_adm1_afr_2021_osv_gtw_fw3 <- readRDS("rds/ml_models/log/rfbm_cov_adm1_afr_2021_osv_gtw_fw3.rds")


## create predictions
train_adm1_afr_2021_osv_bm_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_fw3, train_adm1_afr_2021_osv_bm_fw3)
test_adm1_afr_2021_osv_bm_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_fw3, test_adm1_afr_2021_osv_bm_fw3)

saveRDS(train_adm1_afr_2021_osv_bm_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_bm_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_bm_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_bm_fw3.rds")


train_adm1_afr_2021_osv_cov_fw3$pred <- predict(rf_adm1_afr_2021_osv_cov_fw3, train_adm1_afr_2021_osv_cov_fw3)
test_adm1_afr_2021_osv_cov_fw3$pred <- predict(rf_adm1_afr_2021_osv_cov_fw3, test_adm1_afr_2021_osv_cov_fw3)

saveRDS(train_adm1_afr_2021_osv_cov_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_cov_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_cov_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_cov_fw3.rds")


train_adm1_afr_2021_osv_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_gtw_fw3, train_adm1_afr_2021_osv_gtw_fw3)
test_adm1_afr_2021_osv_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_gtw_fw3, test_adm1_afr_2021_osv_gtw_fw3)

saveRDS(train_adm1_afr_2021_osv_gtw_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_gtw_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_gtw_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_gtw_fw3.rds")


train_adm1_afr_2021_osv_bm_cov_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_cov_fw3, train_adm1_afr_2021_osv_bm_cov_fw3)
test_adm1_afr_2021_osv_bm_cov_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_cov_fw3, test_adm1_afr_2021_osv_bm_cov_fw3)

saveRDS(train_adm1_afr_2021_osv_bm_cov_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_bm_cov_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_bm_cov_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_bm_cov_fw3.rds")


train_adm1_afr_2021_osv_bm_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_gtw_fw3, train_adm1_afr_2021_osv_bm_gtw_fw3)
test_adm1_afr_2021_osv_bm_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_gtw_fw3, test_adm1_afr_2021_osv_bm_gtw_fw3)

saveRDS(train_adm1_afr_2021_osv_bm_gtw_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_bm_gtw_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_bm_gtw_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_bm_gtw_fw3.rds")


train_adm1_afr_2021_osv_cov_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_cov_gtw_fw3, train_adm1_afr_2021_osv_cov_gtw_fw3)
test_adm1_afr_2021_osv_cov_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_cov_gtw_fw3, test_adm1_afr_2021_osv_cov_gtw_fw3)

saveRDS(train_adm1_afr_2021_osv_cov_gtw_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_cov_gtw_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_cov_gtw_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_cov_gtw_fw3.rds")


train_adm1_afr_2021_osv_bm_cov_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_cov_gtw_fw3, train_adm1_afr_2021_osv_bm_cov_gtw_fw3)
test_adm1_afr_2021_osv_bm_cov_gtw_fw3$pred <- predict(rf_adm1_afr_2021_osv_bm_cov_gtw_fw3, test_adm1_afr_2021_osv_bm_cov_gtw_fw3)

saveRDS(train_adm1_afr_2021_osv_bm_cov_gtw_fw3, "rds/predictions/log/train_adm1_afr_2021_osv_bm_cov_gtw_fw3.rds")
saveRDS(test_adm1_afr_2021_osv_bm_cov_gtw_fw3, "rds/predictions/log/test_adm1_afr_2021_osv_bm_cov_gtw_fw3.rds")


# stopCluster(cl)
# registerDoSEQ()

