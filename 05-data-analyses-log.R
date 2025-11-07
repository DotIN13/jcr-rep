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
source("scripts/log/05-data-analysis-admin0-2023-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-africa-sri-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## state-based violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-africa-sbv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## one-sided violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-africa-osv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## non-state violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-africa-nsv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))


## admin0 globally without covariate models
## security-related incidents
source("scripts/log/05-data-analysis-admin0-2023-global-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-global-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-global-sri-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-global-sri-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## state-based violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-global-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-global-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-global-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-global-sbv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## one-sided violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-global-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-global-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-global-osv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-global-osv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## non-state violence fatalities
source("scripts/log/05-data-analysis-admin0-2023-global-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2022-global-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2021-global-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin0-2020-global-nsv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))


## admin1 for africa with covariate models
## security-related incidents
source("scripts/log/05-data-analysis-admin1-2023-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin1-2022-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin1-2021-africa-sri-fw3.R")
source("scripts/log/05-data-analysis-admin1-2020-africa-sri-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## state-based violence fatalities
source("scripts/log/05-data-analysis-admin1-2023-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2022-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2021-africa-sbv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2020-africa-sbv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## one-sided violence fatalities
source("scripts/log/05-data-analysis-admin1-2023-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2022-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2021-africa-osv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2020-africa-osv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))

## non-state violence fatalities
source("scripts/log/05-data-analysis-admin1-2023-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2022-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2021-africa-nsv-fw3.R")
source("scripts/log/05-data-analysis-admin1-2020-africa-nsv-fw3.R")
rm(list = setdiff(ls(), c("cores", "threads", "cl")))


stopCluster(cl)
registerDoSEQ()
