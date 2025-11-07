###################
###################
## create results tables
###################
###################

## clear environment
rm(list = ls())

library(tidyverse)
library(cshapes)
library(sf)
library(caret)
library(ranger)
library(iml)
library(metrica)
library(pdp)
## for parallel computing
library(parallel)
library(doParallel)
## for time keeping
library(tictoc)

tic()

cl <- makeCluster(parallelly::availableCores() - 4, setup_timeout = 0.5) ## convention to leave 1 core for OS
registerDoParallel()

## cshapes for mapping country metrics
cshapes.data <- cshp(date = as.Date("2018-01-01"),
                     useGW = FALSE,
                     dependencies = TRUE) %>%
  mutate(gwno = as.integer(countrycode::countrycode(country_name, origin = "country.name", destination = "gwn", 
                                                    ## microstates not automatically matched
                                                    ## create custom match
                                                    ## http://ksgleditsch.com/data/microstatessystem.dat
                                                    ## http://ksgleditsch.com/data/iisystem.dat
                                                    custom_match = c(c("Andorra" = "232"),
                                                                     c("Antigua & Barbuda" = "58"), 
                                                                     c("Dominica" = "54"), 
                                                                     c("Grenada" = "55"), 
                                                                     c("Kiribati" = "970"),
                                                                     c("Liechtenstein" = "223"),
                                                                     c("Marshall Islands" = "983"),
                                                                     c("Micronesia (Federated states of)" = "987"),
                                                                     c("Monaco" = "221"),
                                                                     c("Nauru" = "971"),
                                                                     c("Palau" = "986"),
                                                                     c("St. Kitts and Nevis" = "60"),
                                                                     c("St. Lucia" = "56"),
                                                                     c("St. Vincent and the Grenadines" = "57"),
                                                                     c("Samoa" = "990"),
                                                                     c("San Marino" = "331"),
                                                                     c("Sao Tome and Principe" = "403"),
                                                                     c("Seychelles" = "591"),
                                                                     c("Tonga" = "972"),
                                                                     c("Tuvalu" = "973"),
                                                                     c("Vanuatu" = "935"),
                                                                     c("Yemen" = "678"))))) %>%
  ## remove unnecessary variables
  dplyr::select(-c(start, end, status, owner, capname, caplong, caplat, b_def, fid))

## this is necessary for plotting later
sf_use_s2(FALSE)


## create object to store all prediction metrics in for regression
## for paper summary plots
metrics.reg.all <- NULL

## create vectors for loops and calculations
col1 <- rep(c(rep("Gtw", 2), rep("Bm", 2), rep("Bm_Gtw", 2)), 2)
col2 <- rep(c(rep(c("Train", "Test"), 3)), 2)
col4 <- c("Gtw", "Bm", "Bm_Gtw")

years <- 2020:2023
viol <- c("sbv", "osv", "nsv", "sri")
fw <- "fw3"

## for code testing
# i <- 2023
# j <- "sbv"
# k <- "fw3"
# l <- 1

for (i in years) {
  
  for (j in viol) {
    
    for (k in fw) {
      
      ## load all data for later
      for (l in 1:length(col1)) {
        outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
        print(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))
        data.temp <- eval(readRDS(paste0("rds/predictions/", paste(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"), "rds", sep = ".")))) %>%
          dplyr::group_by(gwno) %>%
          dplyr::filter(any(eval(as.name(outcome)) == 0) & any(eval(as.name(outcome)) > 0)) %>%
          dplyr::ungroup()
        assign(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"), data.temp)
      }
      rm(l)
      
      
      # ## variable importance tables and plots
      # ## hyperparameter tuning results plots
      # ## pdp plots for gtw variables
      # for (l in 1:length(col4)) {
      #   print(paste("rf_adm0_glob", i, j, tolower(col4[l]), k, sep = "_"))
      #   model.temp <- eval(readRDS(paste0("rds/ml_models/", paste(paste("rf_adm0_glob", i, j, tolower(col4[l]), k, sep = "_"), "rds", sep = "."))))
      #   
      #   ## pdp plots for gtw models
      #   if (col4[l] == "Gtw") {
      #     en_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_en_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     es_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_es_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     fr_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_fr_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     de_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_de_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     pt_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_pt_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     ru_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_ru_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     zh_news <- model.temp %>%
      #       pdp::partial(pred.var = "hits_zh_news") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     en_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_en_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     es_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_es_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     fr_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_fr_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     de_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_de_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     pt_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_pt_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     ru_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_ru_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     zh_web <- model.temp %>%
      #       pdp::partial(pred.var = "hits_zh_web") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     en_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_en") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     es_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_es") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     fr_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_fr") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     de_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_de") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     pt_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_pt") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     ru_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_ru") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     zh_views <- model.temp %>%
      #       pdp::partial(pred.var = "views_zh") %>%
      #       pdp::plotPartial(smooth = TRUE, lwd = 2)
      #     
      #     pdf(paste0("figures/pdp_plots/adm0_glob/", paste("pdp.adm0.glob", i, j, ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", ".")), tolower(col4[l])), k, "pdf", sep = ".")), width = 10, height = 6)
      #     gridExtra::grid.arrange(en_web, es_web, fr_web, de_web,
      #                             pt_web, ru_web, zh_web,
      #                             en_news, es_news, fr_news, de_news,
      #                             pt_news, ru_news, zh_news,
      #                             en_views, es_views, fr_views, de_views,
      #                             pt_views, ru_views, zh_views,
      #                             nrow = 3)
      #     dev.off()
      #     
      #   }
      #   rm(en_web, es_web, fr_web, de_web,
      #      pt_web, ru_web, zh_web,
      #      en_news, es_news, fr_news, de_news,
      #      pt_news, ru_news, zh_news,
      #      en_views, es_views, fr_views, de_views,
      #      pt_views, ru_views, zh_views)
      #   
      #   
      #   ## parameter tuning results plots
      #   plottitle <- paste0("Tuning results: ", ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", "+")), tolower(col4[l])), " (", i, " held out, ", ifelse(j == "sbv", "state-based violence fatalities, ", ifelse(j == "osv", "one-sided violence fatalities, ", ifelse(j == "nsv", "non-state violence fatalities, ", "security-related incidents, "))), ifelse(k == "fw5", "five year window", "three year window"), ")")
      #   toplot <- ggplot(model.temp) +
      #     ggtitle(plottitle) +
      #     theme_bw()
      #   
      #   ggsave(
      #     filename = paste0("figures/tuning_plots/adm0_glob/", paste("tuning.adm0.glob", i, j, ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", ".")), tolower(col4[l])), k, "pdf", sep = ".")),
      #     plot = toplot,
      #     width = 12,
      #     height = 6,
      #     dpi = 300
      #   )
      #   
      #   
      #   ## varimp plots
      #   plottitle <- paste0("Feature importance: ", ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", "+")), tolower(col4[l])), " (", i, " held out, ", ifelse(j == "sbv", "state-based violence fatalities, ", ifelse(j == "osv", "one-sided violence fatalities, ", ifelse(j == "nsv", "non-state violence fatalities, ", "security-related incidents, "))), ifelse(k == "fw5", "five year window", "three year window"), ")")
      #   toplot <- ggplot(varImp(model.temp)) +
      #     ggtitle(plottitle) +
      #     theme_bw()
      #   
      #   ggsave(
      #     filename = paste0("figures/varimp_plots/adm0_glob/", paste("varimp.adm0.glob", i, j, ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", ".")), tolower(col4[l])), k, "pdf", sep = ".")),
      #     plot = toplot,
      #     width = 12,
      #     height = 6,
      #     dpi = 300
      #   )
      #   
      #   
      #   ## varimp tables
      #   vimp <- as.data.frame(as.matrix(varImp(model.temp)$importance))%>%
      #     rownames_to_column("Feature")
      #   
      #   print(xtable::xtable(vimp %>%
      #                          arrange(desc(Overall)),
      #                        caption = paste0("Variable importance: ", ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", "+")), tolower(col4[l]))," (", i, " held-out)"),
      #                        label = paste("tab:varimp.adm0.glob", i, j, ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", ".")), tolower(col4[l])), k, sep = "."),
      #                        digits = 3),
      #         include.rownames = F,
      #         # hline.after = c(0, 0, 2, 4, 6, 8, 10, 12, 14),
      #         # floating.environment = "sidewaystable",
      #         file = paste0("tables/adm0_glob/", paste("varimp.adm0.glob", i, j, ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", ".")), tolower(col4[l])), k, "tex", sep = "."))
      #   )
      #   
      #   # varimp.reg.bm.antigov.fw3
      #   # varimp.reg.cov.antigov.fw3
      #   # varimp.reg.wpc.antigov.fw3
      #   # varimp.reg.bm.wpc.antigov.fw3
      #   # varimp.reg.bm.cov.antigov.fw3
      #   # varimp.reg.cov.wpc.antigov.fw3
      #   # varimp.reg.bm.cov.wpc.antigov.fw3
      #   #
      #   # varimp.reg.bm.progov.fw3
      #   # varimp.reg.cov.progov.fw3
      #   # varimp.reg.wpc.progov.fw3
      #   # varimp.reg.bm.wpc.progov.fw3
      #   # varimp.reg.bm.cov.progov.fw3
      #   # varimp.reg.cov.wpc.progov.fw3
      #   # varimp.reg.bm.cov.wpc.progov.fw3
      #   #
      #   # varimp.reg.bm.antigov.fw5
      #   # varimp.reg.cov.antigov.fw5
      #   # varimp.reg.wpc.antigov.fw5
      #   # varimp.reg.bm.wpc.antigov.fw5
      #   # varimp.reg.bm.cov.antigov.fw5
      #   # varimp.reg.cov.wpc.antigov.fw5
      #   # varimp.reg.bm.cov.wpc.antigov.fw5
      #   #
      #   # varimp.reg.bm.progov.fw5
      #   # varimp.reg.cov.progov.fw5
      #   # varimp.reg.wpc.progov.fw5
      #   # varimp.reg.bm.wpc.progov.fw5
      #   # varimp.reg.bm.cov.progov.fw5
      #   # varimp.reg.cov.wpc.progov.fw5
      #   # varimp.reg.bm.cov.wpc.progov.fw5
      #   
      #   # }
      # }
      # rm(l, m)
      # 
      # 
      # ## create mean absolute error maps for all held out partitions
      # 
      # data.container <- NULL
      # for (l in 1:length(col4)) {
      #   print(paste("test_adm0_glob", i, j, tolower(col4[l]), k, sep = "_"))
      #   
      #   outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
      #   # data.temp.tocheck <- eval(readRDS(paste0("rds/predictions/", paste(paste("test", "cla", tolower(col4[l]), i, j, k, sep = "_"), "rds", sep = "."))))
      #   data.temp <- eval(readRDS(paste0("rds/predictions/", paste(print(paste("test_adm0_glob", i, j, tolower(col4[l]), k, sep = "_")), "rds", sep = ".")))) %>%
      #     group_by(gwno) %>%
      #     summarise(RMSE = sqrt(sum((eval(as.name(outcome)) - pred)^2)/length(eval(as.name(outcome)))),
      #               MAE = sum(abs(eval(as.name(outcome)) - pred)/length(eval(as.name(outcome))))) %>%
      #     ungroup() %>%
      #     mutate(model = ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", "+")), tolower(col4[l])))
      #   data.container <- rbind(data.container, data.temp)
      #   
      # }
      # 
      # group.sf <- left_join(data.container, cshapes.data) %>%
      #   st_as_sf()
      # 
      # toplot <- ggplot(group.sf) +
      #   geom_sf(aes(fill = MAE)) +
      #   scale_fill_viridis_c(option = "plasma",
      #                        direction = -1) +
      #   facet_wrap(~factor(model, c("bm", "gtw", "bm+gtw")), ncol = 3) +
      #   labs(caption = paste0("Mean absolute error for held-out ", i, " data by country.")) +
      #   ggtitle(paste0("Model performances predicting ", ifelse(j == "sbv", "state-based violence fatalities", ifelse(j == "osv", "one-sided violence fatalities", ifelse(j == "nsv", "non-state violence fatalities", "security-related incidents")))), subtitle = paste0("(", length(unique(group.sf$cowcode)), " countries)")) +
      #   theme_bw() +
      #   theme(plot.background = element_blank(),
      #         panel.grid.minor = element_blank(),
      #         panel.grid.major = element_blank(),
      #         panel.border = element_blank(),
      #         axis.ticks = element_blank(),
      #         axis.text = element_blank())
      # 
      # ## change this to 100 dpi and png for size reasons
      # ggsave(
      #   filename = paste0("figures/maps/adm0_glob/", paste("map.adm0.glob", i, j, k, "png", sep = ".")),
      #   plot = toplot,
      #   width = 12,
      #   height = 6,
      #   dpi = 100
      # )
      # 
      # rm(l, data.container, data.temp, group.sf, toplot)
      # 
      # 
      # 
      # ## create line plots for all held out partitions
      # 
      # data.container <- NULL
      # for (l in 1:length(col4)) {
      #   print(paste("test_adm0_glob", i, j, tolower(col4[l]), k, sep = "_"))
      #   
      #   outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
      #   data.temp <- eval(readRDS(paste0("rds/predictions/", paste(print(paste("test_adm0_glob", i, j, tolower(col4[l]), k, sep = "_")), "rds", sep = ".")))) %>%
      #     ## only create lineplots for cases where at least one month saw a "positive" outcome
      #     group_by(country_name, year) %>%
      #     mutate(outcome_positive = any(eval(as.name(outcome)) > 0)) %>%
      #     ungroup() %>%
      #     filter(outcome_positive) %>%
      #     select(-outcome_positive) %>%
      #     group_by(country_name, year, month) %>%
      #     summarise(perror = pred - eval(as.name(outcome)),
      #               actual = eval(as.name(outcome)),
      #               pred = pred) %>%
      #     ungroup() %>%
      #     mutate(model = ifelse(str_detect(col4[l], "_"), tolower(str_replace_all(col4[l], "_", "+")), tolower(col4[l])))
      #   data.container <- rbind(data.container, data.temp)
      #   
      # }
      # data.container$yearmonth <- as.Date(paste(data.container$month, "01", data.container$year, sep = "_"), format = "%m_%d_%Y")
      # 
      # for (m in unique(data.container$country_name)) {
      #   out <- ifelse(j == "sbv", "state-based violence fatalities", ifelse(j == "osv", "one-sided violence fatalities", ifelse(j == "nsv", "non-state violence fatalities", "security-related incidents")))
      #   toplot <- ggplot(subset(data.container, country_name == m), aes(x = yearmonth)) +
      #     ## this one in combination with geom_label
      #     # geom_line(aes(group = model, colour = model, linetype = model), linewidth = 1) +
      #     geom_line(aes(y = pred, colour = model), linewidth = 1) +
      #     geom_point(aes(y = actual), color = "black", size = 2) +
      #     gghighlight::gghighlight(model %in% c("gtw", "bm+gtw")) +
      #     # geom_label(aes(label = model),
      #     #            data = subset(mydata.reg.all, country == countries) %>% filter(yearmonth == max(yearmonth) & model %in% c("bm", "wpc", "bm+wpc", "bm+cov+wpc"))) +
      #     # geom_line(aes(group = model, linetype = model), linewidth = 1) +
      #     labs(x = "Time",
      #          y = paste0("Actual and predicted ", out),
      #          caption = paste0("The plot shows the actual number of ",
      #                           out,
      #                           " as dots and the predicted number of ",
      #                           out,
      #                           " for the models as lines."),
      #          colour = "",
      #          linetype = "") +
      #     scale_x_date(labels = scales::date_format("%m-%Y"),
      #                  date_breaks = "2 months"
      #     ) +
      #     scale_y_continuous(breaks = ~round(unique(pretty(c(subset(data.container, country_name == m)$perror, subset(data.container, country_name == m)$actual))))) +
      #     ggtitle("Prediction error of different models", subtitle = paste(m)) +
      #     theme_bw() +
      #     theme(plot.background = element_blank(),
      #           panel.grid.minor = element_blank(),
      #           panel.grid.major = element_blank(),
      #           panel.border = element_blank())
      #   
      #   ggsave(
      #     filename = paste0("figures/line_plots/adm0_glob/", paste("lp.adm0.glob", i, j, tolower(paste(m)), k, "pdf", sep = ".")),
      #     plot = toplot,
      #     width = 12, # replace with 10 if necessary
      #     height = 6,
      #     dpi = 300
      #   )
      #   
      # }
      # ## the above may have to be uncommented again, not sure
      # rm(l, m, data.container, data.temp, toplot)
      
      
      
      
      
      
      
      ## create regression metrics table
      
      metrics.temp.reg <- as.data.frame(matrix(nrow = 6, ncol = 12,
                                               dimnames = list(NULL, c("Model",
                                                                       "Data",
                                                                       "Observ's",
                                                                       "RMSE",
                                                                       "MAE",
                                                                       "R2",
                                                                       "PCC",
                                                                       "AC",
                                                                       "CCC",
                                                                       "RIA",
                                                                       "RAC",
                                                                       "MIC"))))
      
      
      for (l in 1:nrow(metrics.temp.reg)) {
        outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
        data.temp <- eval(as.name(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))) %>%
          dplyr::group_by(gwno) %>%
          dplyr::filter(any(eval(as.name(outcome)) == 0) & any(eval(as.name(outcome)) > 0)) %>%
          dplyr::ungroup()
        print(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))
        metrics.temp.reg[l,1] <- ifelse(str_detect(col1[l], "_"), tolower(str_replace_all(col1[l], "_", "+")), tolower(col1[l]))
        metrics.temp.reg[l,2] <- tolower(col2[l])
        metrics.temp.reg[l,3] <- nrow(data.temp)
        metrics.temp.reg[l,4] <- metrica::RMSE(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,5] <- metrica::MAE(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,6] <- metrica::R2(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,7] <- metrica::r(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,8] <- metrica::Xa(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,9] <- metrica::CCC(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,10] <- metrica::d1r(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,11] <- metrica::RAC(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg[l,12] <- metrica::MIC(data.temp, eval(as.name(outcome)), pred)
      }
      rm(l, outcome)
      
      
      
      print(xtable::xtable(metrics.temp.reg,
                           caption = paste0("Performance metrics (", i, " held-out): ", ifelse(j == "sbv", "state-based violence fatalities", ifelse(j == "osv", "one-sided violence fatalities", ifelse(j == "nsv", "non-state violence fatalities", "security-related incidents")))),
                           label = paste("tab:metrics.adm0.glob", i, j, k, sep = "."),
                           digits = 3),
            include.rownames = F,
            hline.after = c(0, 0, 2, 4, 6),
            # floating.environment = "sidewaystable",
            file = paste0("tables/onset/adm0_glob/", paste("metrics.adm0.glob", i, j, k, "tex", sep = ".")))
      
      ## this is to replace [ht] with [!htbp] to make four tables fit on one page in the appendix
      input_dir <- "tables/onset/adm0_glob/"
      file_name <- paste0("metrics.adm0.glob.", i, ".", j, ".", k, ".tex")
      file_path <- file.path(input_dir, file_name)
      
      # Read the file contents
      tex_lines <- readLines(file_path)
      
      # Replace the specific line containing "\begin{table}[ht]"
      tex_lines <- gsub("\\\\begin\\{table\\}\\[ht\\]", "\\\\begin{table}[!htbp]", tex_lines)
      
      # Write the modified contents back to the file
      writeLines(tex_lines, file_path)
      
      cat(paste("Updated:", file_name, "\n"))
      
      
      rm(metrics.temp.reg)
      
      
      
      # metrics.temp.reg.red <- as.data.frame(matrix(nrow = 6, ncol = 8,
      #                                              dimnames = list(NULL, c("Model",
      #                                                                      "Data",
      #                                                                      "Observ's",
      #                                                                      "RMSE",
      #                                                                      "MAE",
      #                                                                      # "R2",
      #                                                                      # "PCC",
      #                                                                      "AC",
      #                                                                      "CCC",
      #                                                                      # "RIA",
      #                                                                      "RAC"
      #                                                                      # "MIC"
      #                                              ))))
      #
      #
      # for (l in 1:nrow(metrics.temp.reg.red)) {
      #   data.temp <- eval(as.name(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_")))
      #   print(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))
      #   outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
      #   metrics.temp.reg.red[l,1] <- ifelse(str_detect(col1[l], "_"), tolower(str_replace_all(col1[l], "_", "+")), tolower(col1[l]))
      #   metrics.temp.reg.red[l,2] <- tolower(col2[l])
      #   metrics.temp.reg.red[l,3] <- nrow(data.temp)
      #   metrics.temp.reg.red[l,4] <- metrica::RMSE(data.temp, eval(as.name(outcome)), pred)
      #   metrics.temp.reg.red[l,5] <- metrica::MAE(data.temp, eval(as.name(outcome)), pred)
      #   # metrics.temp.reg.red[l,6] <- metrica::R2(data.temp, eval(as.name(outcome)), pred)
      #   # metrics.temp.reg.red[l,7] <- metrica::r(data.temp, eval(as.name(outcome)), pred)
      #   metrics.temp.reg.red[l,6] <- metrica::Xa(data.temp, eval(as.name(outcome)), pred)
      #   metrics.temp.reg.red[l,7] <- metrica::CCC(data.temp, eval(as.name(outcome)), pred)
      #   # metrics.temp.reg.red[l,10] <- metrica::d1r(data.temp, eval(as.name(outcome)), pred)
      #   metrics.temp.reg.red[l,8] <- metrica::RAC(data.temp, eval(as.name(outcome)), pred)
      #   # metrics.temp.reg.red[l,12] <- metrica::MIC(data.temp, eval(as.name(outcome)), pred)
      # }
      # rm(l, outcome)
      #
      #
      # print(xtable::xtable(metrics.temp.reg.red,
      #                      caption = paste0("Performance metrics (", i, " held-out): ", ifelse(j == "sbv", "state-based violence fatalities", ifelse(j == "osv", "one-sided violence fatalities", ifelse(j == "nsv", "non-state violence fatalities", "security-related incidents")))),
      #                      label = paste("tab:metrics.red.adm0.glob", i, j, k, sep = "."),
      #                      digits = 3),
      #       include.rownames = F,
      #       hline.after = c(0, 0, 2, 4, 6),
      #       # floating.environment = "sidewaystable",
      #       file = paste0("tables/adm0_glob/", paste("metrics.red.adm0.glob", i, j, k, "tex", sep = ".")))
      #
      # rm(metrics.temp.reg)
      
      
      ## plot for paper
      
      metrics.temp.reg.red <- as.data.frame(matrix(nrow = 6, ncol = 8,
                                                   dimnames = list(NULL, c("Model",
                                                                           "Data",
                                                                           "Observ's",
                                                                           "RMSE",
                                                                           "MAE",
                                                                           "AC",
                                                                           "CCC",
                                                                           "RAC"
                                                   ))))
      
      
      for (l in 1:nrow(metrics.temp.reg.red)) {
        outcome <- ifelse(j == "sbv", "sbv_fat_be", ifelse(j == "osv", "osv_fat_be", ifelse(j == "nsv", "nsv_fat_be", "sri_num")))
        data.temp <- eval(as.name(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))) %>%
          dplyr::group_by(gwno) %>%
          dplyr::filter(any(eval(as.name(outcome)) == 0) & any(eval(as.name(outcome)) > 0)) %>%
          dplyr::ungroup()
        print(paste(tolower(col2[l]), "adm0_glob", i, j, tolower(col1[l]), k, sep = "_"))
        metrics.temp.reg.red[l,1] <- ifelse(str_detect(col1[l], "_"), tolower(str_replace_all(col1[l], "_", "+")), tolower(col1[l]))
        metrics.temp.reg.red[l,2] <- tolower(col2[l])
        metrics.temp.reg.red[l,3] <- nrow(data.temp)
        metrics.temp.reg.red[l,4] <- metrica::RMSE(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg.red[l,5] <- metrica::MAE(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg.red[l,6] <- metrica::Xa(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg.red[l,7] <- metrica::CCC(data.temp, eval(as.name(outcome)), pred)
        metrics.temp.reg.red[l,8] <- metrica::RAC(data.temp, eval(as.name(outcome)), pred)
      }
      rm(l, outcome)
      
      
      ## gather all prediction metrics for summary plot
      metrics.reg <- metrics.temp.reg.red %>%
        mutate(year = i,
               type = j,
               fw = k)
      
      if (is.null(metrics.reg.all)) {
        metrics.reg.all <- metrics.reg
      } else {
        metrics.reg.all <- rbind(metrics.reg.all, metrics.reg)
      }
      
      
      metricstoplot.reg.red <- metrics.temp.reg.red %>%
        select(-"Observ's") %>%
        pivot_longer(cols = !c(Data, Model),
                     names_to = "metric",
                     values_to = "value") %>%
        mutate(group = Model)
      
      metricsplot.reg.red <- ggplot(metricstoplot.reg.red, aes(x = value, y = factor(group, levels = c("bm+gtw", "gtw", "bm"))
      )) +
        geom_point() +
        facet_grid(factor(Data, levels = c("train", "test"))~factor(metric, levels = c("RMSE", "MAE", "AC", "CCC", "RAC")), scales = "free_x") +
        theme_bw() +
        labs(x = "Performance", y = NULL) +
        ggh4x::facetted_pos_scales(x = list(
          metricstoplot.reg.red$metric == "AC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.2))),
          metricstoplot.reg.red$metric == "CCC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.2))),
          metricstoplot.reg.red$metric == "RAC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, 0.2)))
        ))
      
      ggsave(
        filename = paste0("figures/metrics_plots/onset/adm0_glob/", paste("mp.adm0.glob", i, j, k, "pdf", sep = ".")),
        metricsplot.reg.red,
        width = 12,
        height = 6,
        dpi = 300
      )
      
      rm(metrics.temp.reg.red, metricstoplot.reg.red, metricsplot.reg.red, metrics.reg)
      
    }
    
  }
  
}
rm(i, j, k)


for (j in viol) {
  
  for (k in fw) {
    
    metrics.temp <- eval(as.name(paste("metrics", "reg", "all", sep = ".")))
    metricstoplot <- metrics.temp %>%
      filter(Data == "test" & fw == k & type == j) %>%
      select(-c("Observ's", "Data", "fw", "type")) %>%
      pivot_longer(cols = !c(year, Model),
                   names_to = "metric",
                   values_to = "value") %>%
      mutate(group = Model)
    
    metricsplot <- ggplot(metricstoplot) +
      geom_point(aes(x = value, y = factor(group, levels = c("bm+gtw", "gtw", "bm")))) +
      geom_point(data = filter(metricstoplot, group %in% c("gtw", "bm+gtw")), aes(x = value, y = factor(group)), shape = 18, colour = "orange", size = 3) +
      facet_grid(factor(year, levels = c("2023", "2022", "2021", "2020"))~factor(metric, levels = c("RMSE", "MAE", "AC", "CCC", "RAC")), scales = "free_x") +
      theme_bw() +
      labs(x = "Performance", y = NULL) +
      ggh4x::facetted_pos_scales(x = list(
        metricstoplot$metric == "RMSE" ~ scale_x_continuous(breaks = scales::pretty_breaks(n = 4)),
        metricstoplot$metric == "MAE" ~ scale_x_continuous(breaks = scales::pretty_breaks(n = 4)),
        metricstoplot$metric == "AC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0.2, 0.8, 0.2))),
        metricstoplot$metric == "CCC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0.2, 0.8, 0.2))),
        metricstoplot$metric == "RAC" ~ scale_x_continuous(limits = c(0, 1), breaks = c(seq(0.2, 0.8, 0.2)))
      ))
    

    ggsave(
      filename = paste0("figures/metrics_plots/onset/adm0_glob/", paste("mp.adm0.glob", j, k, "pdf", sep = ".")),
      metricsplot, 
      width = 12,
      height = 6,
      dpi = 300
    )
    
  }
}
rm(j, k)



stopCluster(cl)
registerDoSEQ()

toc()

