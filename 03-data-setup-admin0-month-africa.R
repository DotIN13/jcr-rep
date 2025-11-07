###################
###################
## code to set up GED/ACLED dataframe on the country-month level
## africa
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(tidyverse)
library(xSub)

## read gadm data
# gadm <- sf::st_read("data/gadm/gadm_410.gpkg")
# ls(gadm)
# unique(gadm$CONTINENT)
# gadmafrica <- gadm %>%
#   filter(CONTINENT == "Africa") #%>%
# saveRDS(gadmafrica, "data/gadm/gadmafrica.rds")
# gadmafrica <- readRDS("data/gadm/gadmafrica.rds")
# 
# gadmprep <- gadmafrica %>%
#   rename(uid = UID,
#          isocode1 = GID_0,
#          isoname1 = NAME_0,
#          isocode2 = GID_1,
#          isoname2 = NAME_1,
#          altname2 = VARNAME_1) %>%
#   select(uid, isocode1, isoname1, isocode2, isoname2, altname2)
# countries_gadm <- unique(gadmprep$isoname1)
# length(unique(gadmafrica$NAME_0))
# unique(gadmafrica$NAME_0)

## set up basic dataframe with all admin 1 regions on the country month level
years <- seq(2008, 2023, 1)
months <- seq(1, 12, 1)

## taken from click click boom
countries <- c("Algeria",
               "Angola",
               "Benin",
               "Botswana",
               "Burkina_Faso",
               "Burundi",
               "Cameroon",
               # "Cape_Verde",
               "Central_African_Republic",
               "Chad",
               # "Comoros",
               "Democratic_Republic_of_the_Congo",
               "Republic_of_the_Congo",
               "Ivory_Coast",
               "Djibouti",
               "Egypt",
               "Equatorial_Guinea",
               "Eritrea",
               "Eswatini",
               "Ethiopia",
               "Gabon",
               "The_Gambia",
               "Ghana",
               "Guinea",
               "Guinea-Bissau",
               "Kenya",
               "Lesotho",
               "Liberia",
               "Libya",
               "Madagascar",
               "Malawi",
               "Mali",
               "Mauritania",
               # "Mauritius",
               "Morocco",
               "Mozambique",
               "Namibia",
               "Niger",
               "Nigeria",
               "Rwanda",
               # "São_Tomé_and_Príncipe",
               "Senegal",
               # "Seychelles",
               "Sierra_Leone",
               "Somalia",
               "South_Africa",
               "South_Sudan",
               "Sudan",
               # "Swaziland",
               "Tanzania",
               "Togo",
               "Tunisia",
               "Uganda",
               "Zambia",
               "Zimbabwe") %>%
  str_replace_all("_", " ") #%>%
#   tolower()
## make gwno list for filtering ged events later
## sao tome principe and seychelles not matched unambiguously (drop?)
## gwn: gleditsch ward numeric, gwc: gleditsch ward character
gwno.codes <- countrycode::countrycode(countries, origin = "country.name", destination = "gwn")

## custom dictionary for adding country names later (ISO codes do not run smoothly via countryname()..)
country_dict <- data.frame(matrix(c("Algeria", "DZA",
                                    "Angola", "AGO",
                                    "Benin", "BEN",
                                    "Botswana", "BWA",
                                    "Burkina Faso", "BFA",
                                    "Burundi", "BDI",
                                    "Cameroon", "CMR",
                                    # "Cape Verde", "CPV",
                                    "Central African Republic", "CAF",
                                    "Chad", "TCD",
                                    # "Comoros", "COM",
                                    "Democratic Republic of the Congo", "COD",
                                    "Republic of the Congo", "COG",
                                    "Ivory Coast", "CIV",
                                    "Djibouti", "DJI",
                                    "Egypt", "EGY",
                                    "Equatorial Guinea", "GNQ",
                                    "Eritrea", "ERI",
                                    "Eswatini", "SWZ",
                                    "Ethiopia", "ETH",
                                    "Gabon", "GAB",
                                    "The Gambia", "GMB",
                                    "Ghana", "GHA",
                                    "Guinea", "GIN",
                                    "Guinea-Bissau", "GNB",
                                    "Kenya", "KEN",
                                    "Lesotho", "LSO",
                                    "Liberia", "LBR",
                                    "Libya", "LBY",
                                    "Madagascar", "MDG",
                                    "Malawi", "MWI",
                                    "Mali", "MLI",
                                    "Mauritania", "MRT",
                                    # "Mauritius", "MUS",
                                    "Morocco", "MAR",
                                    "Mozambique", "MOZ",
                                    "Namibia", "NAM",
                                    "Niger", "NER",
                                    "Nigeria", "NGA",
                                    "Rwanda", "RWA",
                                    # "São_Tomé_and_Príncipe", "STP",
                                    "Senegal", "SEN",
                                    # "Seychelles", "SYC",
                                    "Sierra Leone", "SLE",
                                    "Somalia", "SOM",
                                    "South Africa", "ZAF",
                                    "South Sudan", "SSD",
                                    "Sudan", "SDN",
                                    "Tanzania", "TZA",
                                    "Togo", "TGO",
                                    "Tunisia", "TUN",
                                    "Uganda", "UGA",
                                    "Zambia", "ZMB",
                                    "Zimbabwe", "ZWE"),
                                  ## remember to adjust nrow when adding / deleting countries
                                  nrow = 49, ncol = 2, byrow = TRUE, 
                                  dimnames = list(NULL, c("country_name", "ISO"))))



# countries_gadm_prep <- gadmafrica %>%
#   as.data.frame() %>%
#   filter(NAME_0 %in% countries) #%>%
#   select(region)

# basedf <- NULL

## selecting region above: 10,140 annual obs, 263,640 total obs
## not selecting region above: 12 annual obs, 395,460 total obs

basedf <- expand.grid(country_name = countries,
                      year = years,
                      month = months) %>%
  mutate(iso = countrycode::countrycode(country_name, origin = "country_name", destination = "ISO", custom_dict = country_dict),
         gwno = countrycode::countrycode(iso, origin = "iso3c", destination = "gwn"),
         cow = countrycode::countrycode(iso, origin = "iso3c", destination = "cown"))
sum(is.na(basedf)) ## 0

## create year-month variable for plotting
basedf$yearmonth2 <- as.Date(paste(basedf$month, "01", basedf$year, sep = "_"), format = "%m_%d_%Y")


ged_adm0_africa_frame <- NULL

## data sources to loop over
datasources <- c("ACLED", "GED", "PITF", "SCAD")


for (i in datasources) {
  print(i)
  frame_xsub <- get_xSub_multi(data_source = c(i),
                               ## 6,976 obs
                               country_iso3 = country_dict$ISO,
                               space_unit = "adm0",
                               time_unit = "month") %>%
    ## prio identifiers
    rename(gadm_id = ID_0,
           iso = ISO,
           year = YEAR, 
           yearmonth = YRMO, 
           elev_mean = ELEV_MEAN,
           open_terrain = OPEN_TERRAIN,
           forest = FOREST,
           farmland = FARMLAND,
           nethgr = GREG_NGROUPS,
           nbuiltup = NBUILTUP,
           npetro = NPETRO,
           road_length = ROAD_LENGTH,
           road_density = ROAD_DENSITY
    ) %>%
    select(gadm_id, iso, 
           year, yearmonth,
           elev_mean, open_terrain, forest, farmland, nethgr, nbuiltup, npetro, 
           road_length, road_density) %>%
    filter(year %in% years) %>%
    unique() %>%
    mutate(month = lubridate::month(lubridate::ym(yearmonth)),
           cow = countrycode::countrycode(iso, origin = "iso3c", destination = "cown")) %>%
    select(-c(gadm_id, yearmonth))
  
  if (is.null(ged_adm0_africa_frame)) {
    ged_adm0_africa_frame <- left_join(basedf, frame_xsub, by = c("year", "month", "cow", "iso")) %>% unique()
    print("initialize basedf acled")
  } else {
    ged_adm0_africa_frame <- left_join(ged_adm0_africa_frame, frame_xsub, by = c("year", "month", "cow", "iso")) %>% 
      mutate(elev_mean = coalesce(elev_mean.x, elev_mean.y),
             open_terrain = coalesce(open_terrain.x, open_terrain.y),
             forest = coalesce(forest.x, forest.y),
             farmland = coalesce(farmland.x, farmland.y),
             nethgr = coalesce(nethgr.x, nethgr.y),
             nbuiltup = coalesce(nbuiltup.x, nbuiltup.y),
             npetro = coalesce(npetro.x, npetro.y),
             road_length = coalesce(road_length.x, road_length.y),
             road_density = coalesce(road_density.x, road_density.y)
      ) %>%
      select(-c(elev_mean.x, elev_mean.y,
                open_terrain.x, open_terrain.y,
                forest.x, forest.y,
                farmland.x, farmland.y,
                nethgr.x, nethgr.y,
                nbuiltup.x, nbuiltup.y,
                npetro.x, npetro.y,
                road_length.x, road_length.y,
                road_density.x, road_density.y)) %>%
      unique()
    print("maintain basedf acled")
  }
}



ls(ged_adm0_africa_frame)
sum(is.na(ged_adm0_africa_frame)) ## 0
sapply(ged_adm0_africa_frame, function(x) sum(is.na(x)))
str(ged_adm0_africa_frame$yearmonth)

range(ged_adm0_africa_frame$yearmonth) ## 12 years times 12 months
length(unique(ged_adm0_africa_frame$iso)) ## 49 unique countries


basedf_full <- left_join(basedf, ged_adm0_africa_frame) %>%
  unique()

sum(is.na(basedf_full))
length(unique(basedf_full$gwno))
rm(ged_adm0_africa_frame)

naniar::gg_miss_case(basedf_full, facet = year) + labs(x = "Number of cases")



## bring in peacesciencer data here
library(peacesciencer)
peacesciencer_frame <- create_statedays(system = 'gw') %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  distinct(gwcode, statename, year, month) %>%
  filter(year >= 2008) %>%
  dplyr::filter(gwcode %in% gwno.codes)

acdframe <- create_stateyears(system = 'gw') %>%
  dplyr::filter(gwcode %in% gwno.codes) %>%
  ## add cowcode for merging later
  ## var name ccode
  add_ccode_to_gw() %>%
  ## ucdp onset data
  ## var names: "sumnewconf" "sumonset1"  "sumonset10" "sumonset2"  "sumonset3"  "sumonset5"
  ## The variables returned are the sum of new conflict dyads (should they exist) in a given state-year, 
  ## and the sum of new onset episodes (or new conflicts) that are separated by 
  ## one, two, three, five, or 10 years since the last conflict episode.
  add_ucdp_onsets() %>%
  ## include all issues and all types but extrasystemic
  ## var names: conflict_ids, maxintensity, ucdpongoing, ucdponset
  add_ucdp_acd(type = c("interstate", "intrastate", "II"), 
               issue = c("territory", "government", "both"),
               only_wars = FALSE) %>%
  ## add int gov organizations data from cow
  ## var names: sum igo anytype, sum igo associate, sum igo full, sum igo observer
  ## These are the number of IGOs for which the state is a full member, the number of IGOs for which the state 
  ## is an associate member, the number of IGOs for which the state is an observer, and the number of IGOs for 
  ## which the state is involved in any way (i.e. the sum of the other three columns).
  ## probably not useful here
  add_igos() %>%
  ## add national material capabilities from cow
  ## var names: cinc (composite index of national capabilities), irst, milex, milper, pec, tpop, upop
  add_nmc() %>%
  ## spells and peace years produce the same on default
  # add_spells() %>%
  ## var name: ucdpspell
  add_peace_years() %>%
  ## add leadership info
  ## var names: irregular, dec31obsid & jab1obsid (unique leader id's), leadertransition, nleaders
  add_archigos() %>%
  ## add democracy data
  ## var names: polity2, v2x polyarchy, xm qudsest (double check but xm qudsest presumably best coverage)
  ## here though:v2x polyarchy best coverage, followed by xm qudsest and polity2
  add_democracy() %>%
  ## add fractionalization and polarization data
  ## var names: ethfrac, ethpol, relfrac, relpol
  add_creg_fractionalization() %>%
  ## add population and gdp data
  ## var names: sdpest (surplus domestic product estimate), wbgdp2011est (gdp estimate), 
  ## wbgdppc2011est (gdp per capita estimate), wbpopest (population estimate)
  add_sdp_gdp() %>%
  ## add terrain data
  ## var names: rugged (nunn and puga) and
  ## newlmtnest (must be log trans of how mountainous a state is as calculated by gibler and miller 2014)
  add_rugged_terrain() %>%
  filter(year >= 2008) %>%
  ## remove unnecessary variables
  ## also leave out sdpest, too many NAs which cannot be imputed it seems, gdp is enough
  select(-c(dec31obsid, jan1obsid, polity2, sdpest, 
            sumnewconf, sumonset1, sumonset2, sumonset3, sumonset5, sumonset10,
            maxintensity, conflict_ids, ccode))

range(acdframe$year) ## 2008 - 2021
sum(is.na(acdframe)) ## 7,497
sapply(acdframe, function(x) sum(is.na(x)))


library(mice)
library(Amelia)
library(miceadds)

M <- 100

start1 <- Sys.time()
acdframe_cart <- mice(acdframe, m=M, seed=0815, method='cart')
end1 <- Sys.time()
saveRDS(acdframe_cart, "rds/imputations/acdframe_cart.rds")

end1-start1 ## 12-14 min

sum(is.na(acdframe_cart)) ## 0
acdframe_cart2 <- complete(acdframe_cart)
sum(is.na(acdframe_cart2))
sapply(acdframe_cart2, function(x) sum(is.na(x)))


saveRDS(acdframe_cart2, "rds/imputations/acdframe_cart_complete.rds")

acdframe <- readRDS("rds/imputations/acdframe_cart_complete.rds")
sum(is.na(acdframe)) ## 0
sapply(acdframe, function(x) sum(is.na(x)))
## add information about war onset
create_stateyears(system = 'gw') %>%
  add_ucdp_acd(type = c("interstate", "intrastate", "II"),
               issue = c("territory", "government", "both"),
               only_wars = TRUE) %>%
  add_peace_years() %>%
  rename_at(vars(ucdpongoing:ucdpspell), ~paste0("war_", .)) %>%
  filter(year >= 2008) %>%
  select(-c(war_maxintensity, war_conflict_ids, war_ucdpspell)) %>%
  left_join(acdframe, .) -> acdframe

sum(is.na(acdframe)) ## 196
sapply(acdframe, function(x) sum(is.na(x)))


table(acdframe$war_ucdponset)
table(acdframe$statename, acdframe$war_ucdponset)
table(acdframe$statename, acdframe$war_ucdpongoing)

sum(is.na(acdframe$war_ucdponset)) ## 98
table(acdframe$war_ucdponset)
acdframe$war_ucdponset[is.na(acdframe$war_ucdponset)] = 0

sum(is.na(acdframe$war_ucdpongoing)) ## 98
table(acdframe$war_ucdpongoing)
acdframe$war_ucdpongoing[is.na(acdframe$war_ucdpongoing)] = 0

sum(is.na(acdframe)) ## 0
sapply(acdframe, function(x) sum(is.na(x)))

## create lags for selected variables
acdframe %>%
  rename(gwno = gwcode) %>%
  arrange(gwno, year) %>%
  group_by(gwno) %>%
  ## democracy, gdp/sdp and population estimates
  ## civil conflict and civil war onset, maxintensity
  ## leader transition, irregular transition, number of leaders in a given year
  mutate_at(vars("xm_qudsest", "v2x_polyarchy", "wbgdppc2011est", "wbpopest", 
    "ucdponset", "war_ucdponset", 
    "leadertransition", "irregular", "n_leaders"), 
    list(l1 = ~lag(., 1))) %>%
  rename_at(vars(contains("_l1")),
            ~paste("l1", gsub("_l1", "", .), sep = "_") ) %>%
  ungroup() -> acdframe

sum(is.na(acdframe)) ## 245
sapply(acdframe, function(x) sum(is.na(x)))

sum(is.na(acdframe$l1_leadertransition)) ## 49
table(acdframe$l1_leadertransition)
acdframe$l1_leadertransition[is.na(acdframe$l1_leadertransition)] = 0

sum(is.na(acdframe$l1_irregular)) ## 49
table(acdframe$l1_irregular)
acdframe$l1_irregular[is.na(acdframe$l1_irregular)] = 0

sum(is.na(acdframe$l1_n_leaders)) ## 49
table(acdframe$l1_n_leaders)
acdframe$l1_n_leaders[is.na(acdframe$l1_n_leaders)] = 1

sum(is.na(acdframe$l1_ucdponset)) ## 49
table(acdframe$l1_ucdponset)
acdframe$l1_ucdponset[is.na(acdframe$l1_ucdponset)] = 0

sum(is.na(acdframe$l1_war_ucdponset)) ## 49
table(acdframe$l1_war_ucdponset)
acdframe$l1_war_ucdponset[is.na(acdframe$l1_war_ucdponset)] = 0

sum(is.na(acdframe)) ## 0
sapply(acdframe, function(x) sum(is.na(x)))


peacesciencer_full <- left_join(peacesciencer_frame, acdframe)

sum(is.na(peacesciencer_full)) ## 0

## remove unnecessary objects
rm(peacesciencer_frame)
rm(acdframe)
rm(acdframe2)
rm(acdframe_cart)
rm(acdframe_cart2)


## merge basedf and peacesciencer data
basedf_full_comp <- left_join(basedf_full, peacesciencer_full) %>%
  rename(yearmonth = yearmonth2) %>%
  select(-c(gwcode, statename))

sum(is.na(basedf_full_comp)) ## 23442
sapply(basedf_full_comp, function(x) sum(is.na(x)))
## replace all NAs with 0
basedf_full_comp[is.na(basedf_full_comp)] <- 0

sum(is.na(basedf_full_comp)) ## 23442

naniar::gg_miss_case(basedf_full_comp, facet = year) + labs(x = "Number of cases")

basedf_full_comp <- basedf_full_comp %>%
  group_by(gwno, year) %>%
  fill(names(.), .direction = "updown") %>%
  ungroup()
sum(is.na(basedf_full_comp))

## continue here with updown filling, setting certain variables to zero,
## and imputation again
sum(is.na(basedf_full_comp$leadertransition)) ## 42
table(basedf_full_comp$leadertransition)
basedf_full_comp$leadertransition[is.na(basedf_full_comp$leadertransition)] = 0

sum(is.na(basedf_full_comp$l1_leadertransition)) ## 42
table(basedf_full_comp$l1_leadertransition)
basedf_full_comp$l1_leadertransition[is.na(basedf_full_comp$l1_leadertransition)] = 0

sum(is.na(basedf_full_comp$irregular)) ## 42
table(basedf_full_comp$irregular)
basedf_full_comp$irregular[is.na(basedf_full_comp$irregular)] = 0

sum(is.na(basedf_full_comp$l1_irregular)) ## 42
table(basedf_full_comp$l1_irregular)
basedf_full_comp$l1_irregular[is.na(basedf_full_comp$l1_irregular)] = 0

sum(is.na(basedf_full_comp$n_leaders)) ## 42
table(basedf_full_comp$n_leaders)
basedf_full_comp$n_leaders[is.na(basedf_full_comp$n_leaders)] = 1

sum(is.na(basedf_full_comp$l1_n_leaders)) ## 42
table(basedf_full_comp$l1_n_leaders)
basedf_full_comp$l1_n_leaders[is.na(basedf_full_comp$l1_n_leaders)] = 1

sum(is.na(basedf_full_comp$ucdponset)) ## 42
table(basedf_full_comp$ucdponset)
basedf_full_comp$ucdponset[is.na(basedf_full_comp$ucdponset)] = 0

sum(is.na(basedf_full_comp$l1_ucdponset)) ## 42
table(basedf_full_comp$l1_ucdponset)
basedf_full_comp$l1_ucdponset[is.na(basedf_full_comp$l1_ucdponset)] = 0

sum(is.na(basedf_full_comp$war_ucdponset)) ## 42
table(basedf_full_comp$war_ucdponset)
basedf_full_comp$war_ucdponset[is.na(basedf_full_comp$war_ucdponset)] = 0

sum(is.na(basedf_full_comp$l1_war_ucdponset)) ## 42
table(basedf_full_comp$l1_war_ucdponset)
basedf_full_comp$l1_war_ucdponset[is.na(basedf_full_comp$l1_war_ucdponset)] = 0

sum(is.na(basedf_full_comp$ucdpongoing)) ## 42
table(basedf_full_comp$ucdpongoing)
basedf_full_comp$ucdpongoing[is.na(basedf_full_comp$ucdpongoing)] = 0

sum(is.na(basedf_full_comp$war_ucdpongoing)) ## 42
table(basedf_full_comp$war_ucdpongoing)
basedf_full_comp$war_ucdpongoing[is.na(basedf_full_comp$war_ucdpongoing)] = 0

sum(is.na(basedf_full_comp$ucdpspell)) ## 42
table(basedf_full_comp$ucdpspell)
basedf_full_comp$ucdpspell[is.na(basedf_full_comp$ucdpspell)] = 0

sapply(basedf_full_comp, function(x) sum(is.na(x)))


library(mice)
library(Amelia)
library(miceadds)

M <- 100

start1 <- Sys.time()
basedf_full_comp_cart <- mice(basedf_full_comp, m=M, seed=0815, method='cart')
end1 <- Sys.time()
saveRDS(basedf_full_comp_cart, "rds/imputations/basedf_full_comp_cart.rds")

end1-start1 ## Time difference of 8.24916 hours

sum(is.na(basedf_full_comp_cart)) ## 0
basedf_full_comp_cart2 <- complete(basedf_full_comp_cart)
sum(is.na(basedf_full_comp_cart2))
sapply(basedf_full_comp_cart2, function(x) sum(is.na(x)))


saveRDS(basedf_full_comp_cart2, "rds/imputations/basedf_full_comp_cart_complete.rds")

basedf_full_comp <- readRDS("rds/imputations/basedf_full_comp_cart_complete.rds")


## load and prepare GED data

## ged curated annual release data
ged.raw <- read.csv("data/events/GEDEvent_v23_1.csv")
## ged candidate monthly release data
ged.cand.raw <- read.csv("data/events/GEDEvent_v23_01_23_12.csv")


## create state-based violence dataframe
ged.sbv <- ged.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter state-based violence: government vs rebel group(s)
  dplyr::filter(type_of_violence == 1
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(sbv_fat_be = sum(best),
            sbv_fat_lo = sum(low),
            sbv_fat_hi = sum(high),
            sbv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.sbv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter state-based violence: government vs rebel group(s)
  dplyr::filter(type_of_violence == 1
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(sbv_fat_be = sum(best),
            sbv_fat_lo = sum(low),
            sbv_fat_hi = sum(high),
            sbv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()

## combine curated and candidate data
ged.sbv.full <- rbind(ged.sbv, ged.cand.sbv) %>%
  mutate(sbv_fat_be_log = log1p(sbv_fat_be),
         sbv_fat_lo_log = log1p(sbv_fat_lo),
         sbv_fat_hi_log = log1p(sbv_fat_hi),
         sbv_fat_no_log = log1p(sbv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(
    sbv_fat_be_lag = dplyr::lag(sbv_fat_be, n = 1),
    sbv_fat_lo_lag = dplyr::lag(sbv_fat_lo, n = 1),
    sbv_fat_hi_lag = dplyr::lag(sbv_fat_hi, n = 1),
    sbv_fat_no_lag = dplyr::lag(sbv_fat_no, n = 1),
    sbv_fat_be_change = sbv_fat_be - sbv_fat_be_lag,
    sbv_fat_lo_change = sbv_fat_lo - sbv_fat_lo_lag,
    sbv_fat_hi_change = sbv_fat_hi - sbv_fat_hi_lag,
    sbv_fat_no_change = sbv_fat_no - sbv_fat_no_lag,
    sbv_fat_be_log_lag = dplyr::lag(sbv_fat_be_log, n = 1),
    sbv_fat_lo_log_lag = dplyr::lag(sbv_fat_lo_log, n = 1),
    sbv_fat_hi_log_lag = dplyr::lag(sbv_fat_hi_log, n = 1),
    sbv_fat_no_log_lag = dplyr::lag(sbv_fat_no_log, n = 1),
    sbv_fat_be_log_change = sbv_fat_be_log - sbv_fat_be_log_lag,
    sbv_fat_lo_log_change = sbv_fat_lo_log - sbv_fat_lo_log_lag,
    sbv_fat_hi_log_change = sbv_fat_hi_log - sbv_fat_hi_log_lag,
    sbv_fat_no_log_change = sbv_fat_no_log - sbv_fat_no_log_lag,
    sbv_fat_be_log_change_lag = dplyr::lag(sbv_fat_be_log_change, n = 1),
    sbv_fat_lo_log_change_lag = dplyr::lag(sbv_fat_lo_log_change, n = 1),
    sbv_fat_hi_log_change_lag = dplyr::lag(sbv_fat_hi_log_change, n = 1),
    sbv_fat_no_log_change_lag = dplyr::lag(sbv_fat_no_log_change, n = 1)
  ) %>%
  ungroup() %>%
  replace_na(list(sbv_fat_be_lag = 0,
                  sbv_fat_lo_lag = 0,
                  sbv_fat_hi_lag = 0,
                  sbv_fat_no_lag = 0,
                  sbv_fat_be_log_lag = 0,
                  sbv_fat_lo_log_lag = 0,
                  sbv_fat_hi_log_lag = 0,
                  sbv_fat_no_log_lag = 0,
                  sbv_fat_be_change = 0,
                  sbv_fat_lo_change = 0,
                  sbv_fat_hi_change = 0,
                  sbv_fat_no_change = 0,
                  sbv_fat_be_log_change = 0,
                  sbv_fat_lo_log_change = 0,
                  sbv_fat_hi_log_change = 0,
                  sbv_fat_no_log_change = 0,
                  sbv_fat_be_log_change_lag = 0,
                  sbv_fat_lo_log_change_lag = 0,
                  sbv_fat_hi_log_change_lag = 0,
                  sbv_fat_no_log_change_lag = 0
  ))

sum(is.na(ged.sbv.full))
sapply(ged.sbv.full, function(x) sum(is.na(x)))



## create one-sided violence dataframe
ged.osv <- ged.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 3
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(osv_fat_be = sum(best),
            osv_fat_lo = sum(low),
            osv_fat_hi = sum(high),
            osv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.osv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 3
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(osv_fat_be = sum(best),
            osv_fat_lo = sum(low),
            osv_fat_hi = sum(high),
            osv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()

## combine curated and candidate data
ged.osv.full <- rbind(ged.osv, ged.cand.osv) %>%
  mutate(osv_fat_be_log = log1p(osv_fat_be),
         osv_fat_lo_log = log1p(osv_fat_lo),
         osv_fat_hi_log = log1p(osv_fat_hi),
         osv_fat_no_log = log1p(osv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(
    osv_fat_be_lag = dplyr::lag(osv_fat_be, n = 1),
    osv_fat_lo_lag = dplyr::lag(osv_fat_lo, n = 1),
    osv_fat_hi_lag = dplyr::lag(osv_fat_hi, n = 1),
    osv_fat_no_lag = dplyr::lag(osv_fat_no, n = 1),
    osv_fat_be_change = osv_fat_be - osv_fat_be_lag,
    osv_fat_lo_change = osv_fat_lo - osv_fat_lo_lag,
    osv_fat_hi_change = osv_fat_hi - osv_fat_hi_lag,
    osv_fat_no_change = osv_fat_no - osv_fat_no_lag,
    osv_fat_be_log_lag = dplyr::lag(osv_fat_be_log, n = 1),
    osv_fat_lo_log_lag = dplyr::lag(osv_fat_lo_log, n = 1),
    osv_fat_hi_log_lag = dplyr::lag(osv_fat_hi_log, n = 1),
    osv_fat_no_log_lag = dplyr::lag(osv_fat_no_log, n = 1),
    osv_fat_be_log_change = osv_fat_be_log - osv_fat_be_log_lag,
    osv_fat_lo_log_change = osv_fat_lo_log - osv_fat_lo_log_lag,
    osv_fat_hi_log_change = osv_fat_hi_log - osv_fat_hi_log_lag,
    osv_fat_no_log_change = osv_fat_no_log - osv_fat_no_log_lag,
    osv_fat_be_log_change_lag = dplyr::lag(osv_fat_be_log_change, n = 1),
    osv_fat_lo_log_change_lag = dplyr::lag(osv_fat_lo_log_change, n = 1),
    osv_fat_hi_log_change_lag = dplyr::lag(osv_fat_hi_log_change, n = 1),
    osv_fat_no_log_change_lag = dplyr::lag(osv_fat_no_log_change, n = 1)
  ) %>%
  ungroup() %>%
  replace_na(list(osv_fat_be_lag = 0,
                  osv_fat_lo_lag = 0,
                  osv_fat_hi_lag = 0,
                  osv_fat_no_lag = 0,
                  osv_fat_be_log_lag = 0,
                  osv_fat_lo_log_lag = 0,
                  osv_fat_hi_log_lag = 0,
                  osv_fat_no_log_lag = 0,
                  osv_fat_be_change = 0,
                  osv_fat_lo_change = 0,
                  osv_fat_hi_change = 0,
                  osv_fat_no_change = 0,
                  osv_fat_be_log_change = 0,
                  osv_fat_lo_log_change = 0,
                  osv_fat_hi_log_change = 0,
                  osv_fat_no_log_change = 0,
                  osv_fat_be_log_change_lag = 0,
                  osv_fat_lo_log_change_lag = 0,
                  osv_fat_hi_log_change_lag = 0,
                  osv_fat_no_log_change_lag = 0
  ))

sum(is.na(ged.osv.full))
sapply(ged.osv.full, function(x) sum(is.na(x)))


## create non-state violence dataframe
ged.nsv <- ged.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 2
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(nsv_fat_be = sum(best),
            nsv_fat_lo = sum(low),
            nsv_fat_hi = sum(high),
            nsv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.nsv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 2
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                country,
                gwno,
                region,
                adm_1,
                best,
                low,
                high,
                eventid = id) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
  group_by(gwno, year, month) %>%
  summarise(nsv_fat_be = sum(best),
            nsv_fat_lo = sum(low),
            nsv_fat_hi = sum(high),
            nsv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()

## combine curated and candidate data
ged.nsv.full <- rbind(ged.nsv, ged.cand.nsv) %>%
  mutate(nsv_fat_be_log = log1p(nsv_fat_be),
         nsv_fat_lo_log = log1p(nsv_fat_lo),
         nsv_fat_hi_log = log1p(nsv_fat_hi),
         nsv_fat_no_log = log1p(nsv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(
    nsv_fat_be_lag = dplyr::lag(nsv_fat_be, n = 1),
    nsv_fat_lo_lag = dplyr::lag(nsv_fat_lo, n = 1),
    nsv_fat_hi_lag = dplyr::lag(nsv_fat_hi, n = 1),
    nsv_fat_no_lag = dplyr::lag(nsv_fat_no, n = 1),
    nsv_fat_be_change = nsv_fat_be - nsv_fat_be_lag,
    nsv_fat_lo_change = nsv_fat_lo - nsv_fat_lo_lag,
    nsv_fat_hi_change = nsv_fat_hi - nsv_fat_hi_lag,
    nsv_fat_no_change = nsv_fat_no - nsv_fat_no_lag,
    nsv_fat_be_log_lag = dplyr::lag(nsv_fat_be_log, n = 1),
    nsv_fat_lo_log_lag = dplyr::lag(nsv_fat_lo_log, n = 1),
    nsv_fat_hi_log_lag = dplyr::lag(nsv_fat_hi_log, n = 1),
    nsv_fat_no_log_lag = dplyr::lag(nsv_fat_no_log, n = 1),
    nsv_fat_be_log_change = nsv_fat_be_log - nsv_fat_be_log_lag,
    nsv_fat_lo_log_change = nsv_fat_lo_log - nsv_fat_lo_log_lag,
    nsv_fat_hi_log_change = nsv_fat_hi_log - nsv_fat_hi_log_lag,
    nsv_fat_no_log_change = nsv_fat_no_log - nsv_fat_no_log_lag,
    nsv_fat_be_log_change_lag = dplyr::lag(nsv_fat_be_log_change, n = 1),
    nsv_fat_lo_log_change_lag = dplyr::lag(nsv_fat_lo_log_change, n = 1),
    nsv_fat_hi_log_change_lag = dplyr::lag(nsv_fat_hi_log_change, n = 1),
    nsv_fat_no_log_change_lag = dplyr::lag(nsv_fat_no_log_change, n = 1)
  ) %>%
  ungroup() %>%
  replace_na(list(nsv_fat_be_lag = 0,
                  nsv_fat_lo_lag = 0,
                  nsv_fat_hi_lag = 0,
                  nsv_fat_no_lag = 0,
                  nsv_fat_be_log_lag = 0,
                  nsv_fat_lo_log_lag = 0,
                  nsv_fat_hi_log_lag = 0,
                  nsv_fat_no_log_lag = 0,
                  nsv_fat_be_change = 0,
                  nsv_fat_lo_change = 0,
                  nsv_fat_hi_change = 0,
                  nsv_fat_no_change = 0,
                  nsv_fat_be_log_change = 0,
                  nsv_fat_lo_log_change = 0,
                  nsv_fat_hi_log_change = 0,
                  nsv_fat_no_log_change = 0,
                  nsv_fat_be_log_change_lag = 0,
                  nsv_fat_lo_log_change_lag = 0,
                  nsv_fat_hi_log_change_lag = 0,
                  nsv_fat_no_log_change_lag = 0
  ))

sum(is.na(ged.nsv.full))
sapply(ged.nsv.full, function(x) sum(is.na(x)))

rm(ged.raw)
rm(ged.cand.raw)
rm(ged.sbv)
rm(ged.cand.sbv)
rm(ged.osv)
rm(ged.cand.osv)
rm(ged.nsv)
rm(ged.cand.nsv)

sum(is.na(ged.sbv.full)) ## 0
sum(is.na(ged.osv.full)) ## 0
sum(is.na(ged.nsv.full)) ## 0

basedf_full_comp <- left_join(basedf_full_comp, ged.sbv.full)
basedf_full_comp <- left_join(basedf_full_comp, ged.osv.full)
basedf_full_comp <- left_join(basedf_full_comp, ged.nsv.full)

## acled october 21 release data
acled.raw <- read.csv("data/events/1997-01-01-2024-05-01.csv")

acled.sri.full <- acled.raw %>%
  ## rename a few variables for later
  dplyr::mutate(date = lubridate::dmy(event_date),
                ## three character iso code (iso3)
                ## number iso code (iso)
                ## variable name changed from iso3 to iso
                ISO = iso,
                gwno = countrycode::countrycode(ISO, origin = "iso3n", destination = "gwn",
                                                ## attribute palestinian authority to israel
                                                custom_match = c(c("275" = "666"),
                                                                 ## sao tome and principe
                                                                 c("678" = "403"),
                                                                 c("887" = "678"))),
                month = lubridate::month(date)) %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% gwno.codes) %>%
  ## filter security-related incidents
  dplyr::filter(event_type %in% c("Battles", "Explosions/Remote violence", "Violence against civilians")
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                country,
                gwno,
                event_type,
                fatalities
  ) %>%
  ## group by country-year-month and summarize fatalities and number of sri events
  group_by(gwno, year, month) %>%
  summarise(sri_fat = sum(fatalities),
            sri_num = n()
  ) %>%
  ungroup() %>%
  unique() %>%
  mutate(sri_fat_log = log1p(sri_fat),
         sri_num_log = log1p(sri_num)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  mutate(
    sri_fat_lag = dplyr::lag(sri_fat, n = 1),
    sri_num_lag = dplyr::lag(sri_num, n = 1),
    sri_fat_change = sri_fat - sri_fat_lag,
    sri_num_change = sri_num - sri_num_lag,
    sri_fat_log_lag = dplyr::lag(sri_fat_log, n = 1),
    sri_num_log_lag = dplyr::lag(sri_num_log, n = 1),
    sri_fat_log_change = sri_fat_log - sri_fat_log_lag,
    sri_num_log_change = sri_num_log - sri_num_log_lag,
    sri_fat_log_change_lag = dplyr::lag(sri_fat_log_change),
    sri_num_log_change_lag = dplyr::lag(sri_num_log_change)
  ) %>%
  ungroup() %>%
  replace_na(list(sri_fat_lag = 0,
                  sri_num_lag = 0,
                  sri_fat_change = 0,
                  sri_num_change = 0,
                  sri_fat_log_lag = 0,
                  sri_num_log_lag = 0,
                  sri_fat_log_change = 0,
                  sri_num_log_change = 0,
                  sri_fat_log_change_lag = 0,
                  sri_num_log_change_lag = 0))



rm(acled.raw)
sum(is.na(acled.sri.full)) ## 0
acled.sri.full$gwno <- as.numeric(acled.sri.full$gwno)

basedf_full_comp <- left_join(basedf_full_comp, acled.sri.full)

sum(is.na(basedf_full_comp)) ## 683228
sapply(basedf_full_comp, function(x) sum(is.na(x)))

## replace all NAs with 0
basedf_full_comp[is.na(basedf_full_comp)] <- 0

sum(is.na(basedf_full_comp)) ## 0

saveRDS(basedf_full_comp, "data/basedf_full_comp.rds")



## clear environment
rm(list = ls())

## load libraries
library(tidyverse)

## combine these with gtrends and wikipedia data
basedf_full <- readRDS("data/basedf_full_comp.rds") %>%
  mutate(country_name = as.character(country_name))

page_views_global_all_lang_final <- readRDS("data/wikipedia/new/pageviews_country_global_all_lang_final_20240316.rds") %>%
  mutate(country_name = str_replace_all(country_name, "_", " "),
         month = as.numeric(month),
         year = as.numeric(year))
googletrends_onelangallgprops <- readRDS("data/googletrends/gtrends_country_global_all_lang_all_grops.rds")

sum(is.na(page_views_global_all_lang_final))
sum(is.na(googletrends_onelangallgprops))

gtwikidata_global <- left_join(page_views_global_all_lang_final, googletrends_onelangallgprops) %>%
  mutate(gwno = as.integer(countrycode::countrycode(str_replace_all(country_name, "_", " "), origin = "country.name", destination = "gwn", 
                                                    ## microstates not automatically matched
                                                    ## create custom match
                                                    ## http://ksgleditsch.com/data/microstatessystem.dat
                                                    ## http://ksgleditsch.com/data/iisystem.dat
                                                    custom_match = c(c("Andorra" = "232"),
                                                                     c("Antigua and Barbuda" = "58"), 
                                                                     c("Dominica" = "54"), 
                                                                     c("Grenada" = "55"), 
                                                                     c("Kiribati" = "970"),
                                                                     c("Liechtenstein" = "223"),
                                                                     c("Marshall Islands" = "983"),
                                                                     c("Federated States of Micronesia" = "987"),
                                                                     c("Monaco" = "221"),
                                                                     c("Nauru" = "971"),
                                                                     c("Palau" = "986"),
                                                                     c("Saint Kitts and Nevis" = "60"),
                                                                     c("Saint Lucia" = "56"),
                                                                     c("Saint Vincent and the Grenadines" = "57"),
                                                                     c("Samoa" = "990"),
                                                                     c("San Marino" = "331"),
                                                                     c("São Tomé and Príncipe" = "403"),
                                                                     c("Seychelles" = "591"),
                                                                     c("Tonga" = "972"),
                                                                     c("Tuvalu" = "973"),
                                                                     c("Vanuatu" = "935"),
                                                                     c("Yemen" = "678")))))


sum(is.na(gtwikidata_global))

ls(basedf_full)
str(basedf_full$country_name)
str(gtwikidata_global$country_name)
str(basedf_full$year)
str(gtwikidata_global$year)
str(basedf_full$month)
str(gtwikidata_global$month)
str(basedf_full$gwno)
str(gtwikidata_global$gwno)
fulldata <- left_join(basedf_full, gtwikidata_global) ## , by = c("gwno", "year", "month")

sum(is.na(fulldata)) ## 0
sapply(fulldata, function(x) sum(is.na(x)))


saveRDS(fulldata, "data/fulldata_adm0_africa.rds")
