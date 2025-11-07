###################
###################
## code for analyses on country-month level
## descriptives
###################
###################

## clear environment
rm(list = ls())

## look into library(caretEnsemble)
library(tidyverse)
library(caret)
library(ranger)
library(iml)
## for parallel computing
library(parallel)
library(doParallel)

# cl <- makeCluster(parallelly::availableCores() - 1, setup_timeout = 0.5) ## convention to leave 1 core for OS
# registerDoParallel()


## load full data
data_adm0_glob <- readRDS("data/fulldata_global.rds")
data_adm0_afr <- readRDS("data/fulldata_adm0_africa.rds")
data_adm1_afr <- readRDS("data/fulldata_adm1_africa.rds")

descriptives.df <- as.data.frame(matrix(nrow = 12, ncol = 10, 
                                        dimnames = list(NULL, c("Scope",
                                                                "Unit",
                                                                "Type", 
                                                                "N", 
                                                                "Min", 
                                                                "Median", 
                                                                "Mean", 
                                                                "Max", 
                                                                "IQR",
                                                                "Pct"
                                                                ))))
# ## for later some time
# col1 <- c("SBV", "OSV", "NSV", "SRI")
# for (i in 1:nrow(descriptives.df.admin0.global)) {
#   # data.temp <- eval(as.name(datacol[i]))
#   # print(datacol[i])
#   # preds.temp <- predscol[i]
#   # print(predscol[i])
#   descriptives.df.admin0.global[i,1] <- col1[i]
#   descriptives.df.admin0.global[i,2] <- nrow(data)
#   # descriptives.df.admin0.global[i,3] <- min(data.temp)
#   # descriptives.df.admin0.global[i,4] <-
#   # descriptives.df.admin0.global[i,5] <-
#   # descriptives.df.admin0.global[i,6] <-
#   # descriptives.df.admin0.global[i,7] <-
#   # descriptives.df.admin0.global[i,8] <-
#   # descriptives.df.admin0.global[i,9] <-
#   # descriptives.df.admin0.global[i,10] <-
#   # descriptives.df.admin0.global[i,11] <-
# }
# rm(i)
# paste("data", paste0(tolower(col1[i]), "_fat_be"), sep = "$")

descriptives.df[1,1] <- "Global"
descriptives.df[1,2] <- "Country"
descriptives.df[1,3] <- "SBV"
descriptives.df[1,4] <- nrow(data_adm0_glob)
descriptives.df[1,5] <- min(data_adm0_glob$sbv_fat_be)
descriptives.df[1,6] <- median(data_adm0_glob$sbv_fat_be)
descriptives.df[1,7] <- mean(data_adm0_glob$sbv_fat_be)
descriptives.df[1,8] <- max(data_adm0_glob$sbv_fat_be)
descriptives.df[1,9] <- IQR(data_adm0_glob$sbv_fat_be)
descriptives.df[1,10] <- round(((100/nrow(data_adm0_glob))*length(data_adm0_glob$sbv_fat_be[data_adm0_glob$sbv_fat_be > 0])), 3)

descriptives.df[2,1] <- "Global"
descriptives.df[2,2] <- "Country"
descriptives.df[2,3] <- "OSV"
descriptives.df[2,4] <- nrow(data_adm0_glob)
descriptives.df[2,5] <- min(data_adm0_glob$osv_fat_be)
descriptives.df[2,6] <- median(data_adm0_glob$osv_fat_be)
descriptives.df[2,7] <- mean(data_adm0_glob$osv_fat_be)
descriptives.df[2,8] <- max(data_adm0_glob$osv_fat_be)
descriptives.df[2,9] <- IQR(data_adm0_glob$osv_fat_be)
descriptives.df[2,10] <- round(((100/nrow(data_adm0_glob))*length(data_adm0_glob$osv_fat_be[data_adm0_glob$osv_fat_be > 0])), 3)

descriptives.df[3,1] <- "Global"
descriptives.df[3,2] <- "Country"
descriptives.df[3,3] <- "NSV"
descriptives.df[3,4] <- nrow(data_adm0_glob)
descriptives.df[3,5] <- min(data_adm0_glob$nsv_fat_be)
descriptives.df[3,6] <- median(data_adm0_glob$nsv_fat_be)
descriptives.df[3,7] <- mean(data_adm0_glob$nsv_fat_be)
descriptives.df[3,8] <- max(data_adm0_glob$nsv_fat_be)
descriptives.df[3,9] <- IQR(data_adm0_glob$nsv_fat_be)
descriptives.df[3,10] <- round(((100/nrow(data_adm0_glob))*length(data_adm0_glob$nsv_fat_be[data_adm0_glob$nsv_fat_be > 0])), 3)

descriptives.df[4,1] <- "Global"
descriptives.df[4,2] <- "Country"
descriptives.df[4,3] <- "SRI"
descriptives.df[4,4] <- nrow(data_adm0_glob)
descriptives.df[4,5] <- min(data_adm0_glob$sri_num)
descriptives.df[4,6] <- median(data_adm0_glob$sri_num)
descriptives.df[4,7] <- mean(data_adm0_glob$sri_num)
descriptives.df[4,8] <- max(data_adm0_glob$sri_num)
descriptives.df[4,9] <- IQR(data_adm0_glob$sri_num)
descriptives.df[4,10] <- round(((100/nrow(data_adm0_glob))*length(data_adm0_glob$sri_num[data_adm0_glob$sri_num > 0])), 3)


descriptives.df[5,1] <- "Africa"
descriptives.df[5,2] <- "Country"
descriptives.df[5,3] <- "SBV"
descriptives.df[5,4] <- nrow(data_adm0_afr)
descriptives.df[5,5] <- min(data_adm0_afr$sbv_fat_be)
descriptives.df[5,6] <- median(data_adm0_afr$sbv_fat_be)
descriptives.df[5,7] <- mean(data_adm0_afr$sbv_fat_be)
descriptives.df[5,8] <- max(data_adm0_afr$sbv_fat_be)
descriptives.df[5,9] <- IQR(data_adm0_afr$sbv_fat_be)
descriptives.df[5,10] <- round(((100/nrow(data_adm0_afr))*length(data_adm0_afr$sbv_fat_be[data_adm0_afr$sbv_fat_be > 0])), 3)

descriptives.df[6,1] <- "Africa"
descriptives.df[6,2] <- "Country"
descriptives.df[6,3] <- "OSV"
descriptives.df[6,4] <- nrow(data_adm0_afr)
descriptives.df[6,5] <- min(data_adm0_afr$osv_fat_be)
descriptives.df[6,6] <- median(data_adm0_afr$osv_fat_be)
descriptives.df[6,7] <- mean(data_adm0_afr$osv_fat_be)
descriptives.df[6,8] <- max(data_adm0_afr$osv_fat_be)
descriptives.df[6,9] <- IQR(data_adm0_afr$osv_fat_be)
descriptives.df[6,10] <- round(((100/nrow(data_adm0_afr))*length(data_adm0_afr$osv_fat_be[data_adm0_afr$osv_fat_be > 0])), 3)

descriptives.df[7,1] <- "Africa"
descriptives.df[7,2] <- "Country"
descriptives.df[7,3] <- "NSV"
descriptives.df[7,4] <- nrow(data_adm0_afr)
descriptives.df[7,5] <- min(data_adm0_afr$nsv_fat_be)
descriptives.df[7,6] <- median(data_adm0_afr$nsv_fat_be)
descriptives.df[7,7] <- mean(data_adm0_afr$nsv_fat_be)
descriptives.df[7,8] <- max(data_adm0_afr$nsv_fat_be)
descriptives.df[7,9] <- IQR(data_adm0_afr$nsv_fat_be)
descriptives.df[7,10] <- round(((100/nrow(data_adm0_afr))*length(data_adm0_afr$nsv_fat_be[data_adm0_afr$nsv_fat_be > 0])), 3)

descriptives.df[8,1] <- "Africa"
descriptives.df[8,2] <- "Country"
descriptives.df[8,3] <- "SRI"
descriptives.df[8,4] <- nrow(data_adm0_afr)
descriptives.df[8,5] <- min(data_adm0_afr$sri_num)
descriptives.df[8,6] <- median(data_adm0_afr$sri_num)
descriptives.df[8,7] <- mean(data_adm0_afr$sri_num)
descriptives.df[8,8] <- max(data_adm0_afr$sri_num)
descriptives.df[8,9] <- IQR(data_adm0_afr$sri_num)
descriptives.df[8,10] <- round(((100/nrow(data_adm0_afr))*length(data_adm0_afr$sri_num[data_adm0_afr$sri_num > 0])), 3)


descriptives.df[9,1] <- "Africa"
descriptives.df[9,2] <- "Province"
descriptives.df[9,3] <- "SBV"
descriptives.df[9,4] <- nrow(data_adm1_afr)
descriptives.df[9,5] <- min(data_adm1_afr$sbv_fat_be)
descriptives.df[9,6] <- median(data_adm1_afr$sbv_fat_be)
descriptives.df[9,7] <- mean(data_adm1_afr$sbv_fat_be)
descriptives.df[9,8] <- max(data_adm1_afr$sbv_fat_be)
descriptives.df[9,9] <- IQR(data_adm1_afr$sbv_fat_be)
descriptives.df[9,10] <- round(((100/nrow(data_adm1_afr))*length(data_adm1_afr$sbv_fat_be[data_adm1_afr$sbv_fat_be > 0])), 3)

descriptives.df[10,1] <- "Africa"
descriptives.df[10,2] <- "Province"
descriptives.df[10,3] <- "OSV"
descriptives.df[10,4] <- nrow(data_adm1_afr)
descriptives.df[10,5] <- min(data_adm1_afr$osv_fat_be)
descriptives.df[10,6] <- median(data_adm1_afr$osv_fat_be)
descriptives.df[10,7] <- mean(data_adm1_afr$osv_fat_be)
descriptives.df[10,8] <- max(data_adm1_afr$osv_fat_be)
descriptives.df[10,9] <- IQR(data_adm1_afr$osv_fat_be)
descriptives.df[10,10] <- round(((100/nrow(data_adm1_afr))*length(data_adm1_afr$osv_fat_be[data_adm1_afr$osv_fat_be > 0])), 3)

descriptives.df[11,1] <- "Africa"
descriptives.df[11,2] <- "Province"
descriptives.df[11,3] <- "NSV"
descriptives.df[11,4] <- nrow(data_adm1_afr)
descriptives.df[11,5] <- min(data_adm1_afr$nsv_fat_be)
descriptives.df[11,6] <- median(data_adm1_afr$nsv_fat_be)
descriptives.df[11,7] <- mean(data_adm1_afr$nsv_fat_be)
descriptives.df[11,8] <- max(data_adm1_afr$nsv_fat_be)
descriptives.df[11,9] <- IQR(data_adm1_afr$nsv_fat_be)
descriptives.df[11,10] <- round(((100/nrow(data_adm1_afr))*length(data_adm1_afr$nsv_fat_be[data_adm1_afr$nsv_fat_be > 0])), 3)

descriptives.df[12,1] <- "Africa"
descriptives.df[12,2] <- "Province"
descriptives.df[12,3] <- "SRI"
descriptives.df[12,4] <- nrow(data_adm1_afr)
descriptives.df[12,5] <- min(data_adm1_afr$sri_num)
descriptives.df[12,6] <- median(data_adm1_afr$sri_num)
descriptives.df[12,7] <- mean(data_adm1_afr$sri_num)
descriptives.df[12,8] <- max(data_adm1_afr$sri_num)
descriptives.df[12,9] <- IQR(data_adm1_afr$sri_num)
descriptives.df[12,10] <- round(((100/nrow(data_adm1_afr))*length(data_adm1_afr$sri_num[data_adm1_afr$sri_num > 0])), 3)


print(xtable::xtable(descriptives.df,
                     caption = "Descriptive statistics (target variables)",
                     label = "tab:descr",
                     digits = 3), 
      include.rownames = F,
      hline.after = c(0, 0, 4, 8, 12),
      # floating.environment = "sidewaystable",
      file = "tables/descriptives.main.tex")


# print(xtable::xtable(descriptives.df.admin0.global,
#                      caption = "Descriptive statistics (global country-month)",
#                      label = "tab:descrglobal",
#                      digits = 0
#                      ), 
#       include.rownames = F)
# rm(descriptives.df.admin0.global)
# rm(data)
# 
# data <- readRDS("data/fulldata_admin1_africa.rds") %>%
#   filter(year <= 2020)
# 
# descriptives.df.admin1.africa <- as.data.frame(matrix(nrow = 4, ncol = 10, 
#                                                       dimnames = list(NULL, c("Type of violence", 
#                                                                               "N",
#                                                                               "Min", 
#                                                                               "Median", 
#                                                                               "Mean", 
#                                                                               "Max", 
#                                                                               "IQR",
#                                                                               "Number",
#                                                                               "Percentage",
#                                                                               "# no overlap"))))
# 
# 
# descriptives.df.admin1.africa[1,1] <- "SBV"
# descriptives.df.admin1.africa[1,2] <- nrow(data)
# descriptives.df.admin1.africa[1,3] <- min(data$sbv_fat_be)
# descriptives.df.admin1.africa[1,4] <- median(data$sbv_fat_be)
# descriptives.df.admin1.africa[1,5] <- mean(data$sbv_fat_be)
# descriptives.df.admin1.africa[1,6] <- max(data$sbv_fat_be)
# descriptives.df.admin1.africa[1,7] <- IQR(data$sbv_fat_be, na.rm = T)
# descriptives.df.admin1.africa[1,8] <- length(data$sbv_fat_be[data$sbv_fat_be > 0])
# descriptives.df.admin1.africa[1,9] <- round(((100/nrow(data))*length(data$sbv_fat_be[data$sbv_fat_be > 0])), 3)
# descriptives.df.admin1.africa[1,10] <- length(data$sbv_fat_be[data$sbv_fat_be > 0 & data$sri_num == 0 & data$osv_fat_be == 0 & data$nsv_fat_be])
# 
# descriptives.df.admin1.africa[2,1] <- "OSV"
# descriptives.df.admin1.africa[2,2] <- nrow(data)
# descriptives.df.admin1.africa[2,3] <- min(data$osv_fat_be)
# descriptives.df.admin1.africa[2,4] <- median(data$osv_fat_be)
# descriptives.df.admin1.africa[2,5] <- mean(data$osv_fat_be)
# descriptives.df.admin1.africa[2,6] <- max(data$osv_fat_be)
# descriptives.df.admin1.africa[2,7] <- IQR(data$osv_fat_be, na.rm = T)
# descriptives.df.admin1.africa[2,8] <- length(data$osv_fat_be[data$osv_fat_be > 0])
# descriptives.df.admin1.africa[2,9] <- round(((100/nrow(data))*length(data$osv_fat_be[data$osv_fat_be > 0])), 3)
# descriptives.df.admin1.africa[2,10] <- length(data$osv_fat_be[data$osv_fat_be > 0 & data$sbv_fat_be == 0 & data$sri_num == 0 & data$nsv_fat_be])
# 
# descriptives.df.admin1.africa[3,1] <- "NSV"
# descriptives.df.admin1.africa[3,2] <- nrow(data)
# descriptives.df.admin1.africa[3,3] <- min(data$nsv_fat_be)
# descriptives.df.admin1.africa[3,4] <- median(data$nsv_fat_be)
# descriptives.df.admin1.africa[3,5] <- mean(data$nsv_fat_be)
# descriptives.df.admin1.africa[3,6] <- max(data$nsv_fat_be)
# descriptives.df.admin1.africa[3,7] <- IQR(data$nsv_fat_be, na.rm = T)
# descriptives.df.admin1.africa[3,8] <- length(data$nsv_fat_be[data$nsv_fat_be > 0])
# descriptives.df.admin1.africa[3,9] <- round(((100/nrow(data))*length(data$nsv_fat_be[data$nsv_fat_be > 0])), 3)
# descriptives.df.admin1.africa[3,10] <- length(data$nsv_fat_be[data$nsv_fat_be > 0 & data$sbv_fat_be == 0 & data$osv_fat_be == 0 & data$sri_num])
# 
# descriptives.df.admin1.africa[4,1] <- "SRI"
# descriptives.df.admin1.africa[4,2] <- nrow(data)
# descriptives.df.admin1.africa[4,3] <- min(data$sri_num)
# descriptives.df.admin1.africa[4,4] <- median(data$sri_num)
# descriptives.df.admin1.africa[4,5] <- mean(data$sri_num)
# descriptives.df.admin1.africa[4,6] <- max(data$sri_num)
# descriptives.df.admin1.africa[4,7] <- IQR(data$sri_num, na.rm = T)
# descriptives.df.admin1.africa[4,8] <- length(data$sri_num[data$sri_num > 0])
# descriptives.df.admin1.africa[4,9] <- round(((100/nrow(data))*length(data$sri_num[data$sri_num > 0])), 3)
# descriptives.df.admin1.africa[4,10] <- length(data$sri_num[data$sri_num > 0 & data$sbv_fat_be == 0 & data$osv_fat_be == 0 & data$nsv_fat_be])
# 
# 
# 
# ## independent variables
# ## load full data
# data <- readRDS("data/fulldata_global.rds")
# 
# descriptives.df.admin0.global <- as.data.frame(matrix(nrow = 7, ncol = 7, 
#                                                       dimnames = list(NULL, c("Variable", 
#                                                                               "N", 
#                                                                               "Min", 
#                                                                               "Median", 
#                                                                               "Mean", 
#                                                                               "Max", 
#                                                                               "IQR"
#                                                                               # "Number",
#                                                                               # "Percentage",
#                                                                               # "# no overlap"
#                                                                               ))))
# 
# lang <- c("en", "fr", "es", "de", "pt", "ru", "zh")
# for (i in 1:nrow(descriptives.df.admin0.global)) {
#   # attach(data)
#   descriptives.df.admin0.global[i,1] <- paste0("Page views (", lang[i], ")")
#   descriptives.df.admin0.global[i,2] <- nrow(data)
#   descriptives.df.admin0.global[i,3] <- min(data[, paste("views", lang[i], sep = "_")])
#   descriptives.df.admin0.global[i,4] <- median(data[, paste("views", lang[i], sep = "_")])
#   descriptives.df.admin0.global[i,5] <- mean(data[, paste("views", lang[i], sep = "_")])
#   descriptives.df.admin0.global[i,6] <- max(data[, paste("views", lang[i], sep = "_")])
#   descriptives.df.admin0.global[i,7] <- IQR(data[, paste("views", lang[i], sep = "_")])
#   # detach(data)
# }
# rm(i)
# 
# 
# africa <- readRDS("data/fulldata_adm1_africa.rds")
# 
# descriptives.df.admin0.africa <- as.data.frame(matrix(nrow = 7, ncol = 7, 
#                                                       dimnames = list(NULL, c("Variable", 
#                                                                               "N", 
#                                                                               "Min", 
#                                                                               "Median", 
#                                                                               "Mean", 
#                                                                               "Max", 
#                                                                               "IQR"
#                                                                               # "Number",
#                                                                               # "Percentage",
#                                                                               # "# no overlap"
#                                                       ))))
# 
# ## can (and should) be looped over both languages and props (web, news, etc)
# lang <- c("en", "fr", "es",
#           "de", "pt", "ru", "zh"
#           )
# for (i in 1:nrow(descriptives.df.admin0.africa)) {
#   # attach(data)
#   descriptives.df.admin0.africa[i,1] <- paste0("Hits (", lang[i], ")")
#   descriptives.df.admin0.africa[i,2] <- nrow(africa)
#   descriptives.df.admin0.africa[i,3] <- min(africa[, paste("hits", lang[i], "web", sep = "_")])
#   descriptives.df.admin0.africa[i,4] <- median(africa[, paste("hits", lang[i], "web", sep = "_")])
#   descriptives.df.admin0.africa[i,5] <- mean(africa[, paste("hits", lang[i], "web", sep = "_")])
#   descriptives.df.admin0.africa[i,6] <- max(africa[, paste("hits", lang[i], "web", sep = "_")])
#   descriptives.df.admin0.africa[i,7] <- IQR(africa[, paste("hits", lang[i], "web", sep = "_")])
#   # detach(data)
# }
# rm(i)



bm_adm0_glob_descriptives <- readRDS("rds/data/data_adm0_glob_sbv_bm.rds") %>%
  select(- c(country_name,
             gwno,
             month,
             year,
             yearmonth
  )) %>%
  as.data.frame()
stargazer::stargazer(bm_adm0_glob_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all benchmark model variables (Country - Global)",
                     label = "tab:bm.adm0.glob.descr",
                     style = "ajps",
                     out = "tables/bm.adm0.glob.descriptives.tex")
## fix .000 inconsistencies
file_path <- "tables/bm.adm0.glob.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)


bm_adm0_afr_descriptives <- readRDS("rds/data/data_adm0_afr_sbv_bm.rds") %>%
  select(- c(country_name,
             gwno,
             month,
             year,
             yearmonth
  )) %>%
  as.data.frame()
stargazer::stargazer(bm_adm0_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all benchmark model variables (Country - Africa)",
                     label = "tab:bm.adm0.afr.descr",
                     style = "ajps",
                     out = "tables/bm.adm0.afr.descriptives.tex")
## fix .000 inconsistencies
file_path <- "tables/bm.adm0.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)



bm_adm1_afr_descriptives <- readRDS("rds/data/data_adm1_afr_sbv_bm.rds") %>%
  select(- c(gadmname,
             isocode2full,
             month,
             year
  )) %>%
  as.data.frame()
stargazer::stargazer(bm_adm1_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all benchmark model variables (Province - Africa)",
                     label = "tab:bm.adm1.afr.descr",
                     style = "ajps",
                     out = "tables/bm.adm1.afr.descriptives.tex")
## fix .000 inconsistencies
file_path <- "tables/bm.adm1.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)



cov_adm0_afr_descriptives <- readRDS("rds/data/data_adm0_afr_sbv_cov.rds") %>%
  select(- c(sbv_fat_be,
             country_name,
             gwno,
             month,
             year,
             yearmonth
  )) %>%
  as.data.frame()
## name variables
varnames <- c("Military Capabilities Index", 
              "Mean elevation", 
              "Ethnic fractionalization", 
              "Ethnic polarization",
              "Farmland",
              "Forest",
              "Irregular leadership change",
              "Iron + steel prod",
              "Mil expenditure",
              "Mil personnel",
              "Number of leaders",
              "Number builtup areas",
              "Number ethnic groups",
              "Pct mountainous terrain (ln)",
              "Number petroleum fields",
              "Open terrain",
              "Private energy consumption",
              "Religious fractionalization",
              "Religious polarization",
              "Road density",
              "Road length",
              "Terrain ruggedness index",
              "Sum IGO (any)",
              "Sum IGO (associate)",
              "Sum IGO (full)",
              "Sum IGO (observer)",
              "Total pop (CoW)",
              "Urban pop(CoW)",
              "Polyarchy index",
              "GDP (WB)",
              "GDPpc (WB)",
              "Population (WB)",
              "Unified dem scores",
              "Irregular leader change lag",
              "Leader transition lag",
              "Number leaders lag",
              "Polyarchy lag",
              "GDPpc (WB) lag",
              "GDP (WB) lag",
              "Unified dem scores lag")
stargazer::stargazer(cov_adm0_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all covariate model variables (Country - Africa)",
                     label = "tab:cov.adm0.afr.descr",
                     style = "ajps",
                     covariate.labels = varnames,
                     out = "tables/cov.adm0.afr.descriptives.tex")
file_path <- "tables/cov.adm0.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)


cov_adm1_afr_descriptives <- readRDS("rds/data/data_adm1_afr_sbv_cov.rds") %>%
  select(- c(sbv_fat_be,
             gadmname,
             isocode2full,
             month,
             year
  )) %>%
  as.data.frame()
varnames <- c("Mean elevation", 
              "Farmland",
              "Forest",
              "GDP ppp", 
              "Number builtup areas",
              "Number ethnic groups",
              "Nighttime lights",
              "Number petroleum fields",
              "Open terrain",
              "Population sum",
              "Rainfall",
              "Road density",
              "Road length",
              "Temperature")
stargazer::stargazer(cov_adm1_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all covariate model variables (Province - Africa)",
                     label = "tab:cov.adm1.afr.descr",
                     style = "ajps",
                     covariate.labels = varnames,
                     out = "tables/cov.adm1.afr.descriptives.tex")
file_path <- "tables/cov.adm1.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)



gtw_adm0_glob_descriptives <- readRDS("rds/data/data_adm0_glob_sbv_gtw.rds") %>%
  select(- c(sbv_fat_be,
             country_name,
             gwno,
             month,
             year,
             yearmonth
  )) %>%
  as.data.frame()
stargazer::stargazer(gtw_adm0_glob_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all Google trends and Wikipedia model variables (Country - Global)",
                     label = "tab:gtw.adm0.glob.descr",
                     style = "ajps",
                     out = "tables/gtw.adm0.glob.descriptives.tex")
file_path <- "tables/gtw.adm0.glob.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)


gtw_adm0_afr_descriptives <- readRDS("rds/data/data_adm0_afr_sbv_gtw.rds") %>%
  select(- c(sbv_fat_be,
             country_name,
             gwno,
             month,
             year,
             yearmonth
  )) %>%
  as.data.frame()
stargazer::stargazer(gtw_adm0_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all Google trends and Wikipedia model variables (Country - Africa)",
                     label = "tab:gtw.adm0.afr.descr",
                     style = "ajps",
                     digits = 3,
                     out = "tables/gtw.adm0.afr.descriptives.tex")

## fix .000 inconsistencies
file_path <- "tables/gtw.adm0.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)



gtw_adm1_afr_descriptives <- readRDS("rds/data/data_adm1_afr_sbv_gtw.rds") %>%
  select(- c(sbv_fat_be,
             gadmname,
             isocode2full,
             month,
             year
  )) %>%
  as.data.frame()
stargazer::stargazer(gtw_adm1_afr_descriptives,
                     summary.stat = c("n", "min", "mean", "median", "max", "sd"),
                     title = "Summary statistics for all Google trends and Wikipedia model variables (Province - Africa)",
                     label = "tab:gtw.adm1.afr.descr",
                     style = "ajps",
                     out = "tables/gtw.adm1.afr.descriptives.tex")
file_path <- "tables/gtw.adm1.afr.descriptives.tex"
tex_content <- readLines(file_path)

tex_content <- str_replace_all(tex_content, "\\.000(?![0-9])", "")

writeLines(tex_content, file_path)




## illustrate plots

## clear environment
rm(list = ls())

## look into library(caretEnsemble)
library(tidyverse)
## to avoid scientific notation on scales
options(scipen = 999)

## load full data
data <- readRDS("data/fulldata_global.rds")

ukraine <- data %>%
  filter(country_name == "Ukraine")

# Calculate scaling factors
max_first  <- max(ukraine$hits_en_web)   # Specify max of first y axis
max_second <- max(ukraine$views_en)      # Specify max of second y axis
min_first  <- min(ukraine$hits_en_web)   # Specify min of first y axis
min_second <- min(ukraine$views_en)      # Specify min of second y axis

# Scale and shift variables calculated based on desired mins and maxes
scale = (max_second - min_second)/(max_first - min_first)
shift = min_first - min_second

# Function to scale secondary axis
scale_function <- function(x, scale, shift){
  return ((x)*scale - shift)
}

# Function to scale secondary variable values
inv_scale_function <- function(x, scale, shift){
  return ((x + shift)/scale)
}

p5 <- ggplot(ukraine, aes(x = yearmonth)) +
  # Web data
  geom_line(aes(y = hits_en_web, color = "English", linetype = "web"), linewidth = 1) +
  geom_line(aes(y = hits_ru_web, color = "Russian", linetype = "web"), linewidth = 1) + 

  # News data
  geom_line(aes(y = hits_en_news, color = "English", linetype = "news"), linewidth = 1) +
  geom_line(aes(y = hits_ru_news, color = "Russian", linetype = "news"), linewidth = 1) + 

  # Views data (scaled)
  geom_line(aes(y = inv_scale_function(views_en, scale, shift), color = "English", linetype = "views"), linewidth = 1) +
  geom_line(aes(y = inv_scale_function(views_ru, scale, shift), color = "Russian", linetype = "views"), linewidth = 1) + 

  # Define color for each language
  scale_color_manual(name = "Language", 
                     values = c("English" = "red", 
                                "Russian" = "orange2"
                     )) +
  
  # Define line types for web (solid), news (dashed), and views (dotted)
  scale_linetype_manual(name = "Variable", 
                        values = c("web" = "solid", "news" = "dashed", "views" = "dotted")) +
  
  # Set up dual y-axes
  scale_y_continuous(name = "Google hits", 
                     sec.axis = sec_axis(~scale_function(., scale, shift), 
                                         name = "Page views")) +
  
  ggtitle("Ukraine") +
  xlab("Time") + 
  theme_bw() + 
  geom_vline(xintercept = as.Date("2022-02-01"), 
             linetype = "solid", 
             color = "black", 
             size = 1) +
  annotate("text", x = as.Date("2022-02-01"), y = Inf, 
           label = "Invasion", 
           angle = 45, 
           vjust = 1.5, 
           hjust = 1.5, 
           size = 4) +
  geom_vline(xintercept = as.Date("2014-03-01"), 
             linetype = "solid", 
             color = "black", 
             size = 1) +
  annotate("text", x = as.Date("2014-03-01"), y = Inf, 
           label = "Crimea", 
           angle = 45, 
           vjust = 1.5, 
           hjust = 1.5, 
           size = 4)
p5
ggsave(
  "figures/ukraine_comp.pdf",
  p5, 
  dpi = 120
)

