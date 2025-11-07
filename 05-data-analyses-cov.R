## clear environment
rm(list = ls())

## look into library(caretEnsemble)
library(dplyr)
library(caret)
library(ranger)
# library(iml)
## for parallel computing
library(parallel)
library(doParallel)

cores <- 4
threads <- 5
cl <- makeCluster(cores)
registerDoParallel(cl)

## admin0 for africa with covariate models
## security-related incidents
source("scripts/cov/05-data-analysis-admin0-2023-africa-sri-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2022-africa-sri-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2021-africa-sri-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2020-africa-sri-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## state-based violence fatalities
source("scripts/cov/05-data-analysis-admin0-2023-africa-sbv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2022-africa-sbv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2021-africa-sbv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2020-africa-sbv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## one-sided violence fatalities
source("scripts/cov/05-data-analysis-admin0-2023-africa-osv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2022-africa-osv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2021-africa-osv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2020-africa-osv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## non-state violence fatalities
source("scripts/cov/05-data-analysis-admin0-2023-africa-nsv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2022-africa-nsv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2021-africa-nsv-fw3.R")
source("scripts/cov/05-data-analysis-admin0-2020-africa-nsv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))


stopCluster(cl)
registerDoSEQ()
