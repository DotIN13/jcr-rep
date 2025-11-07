###################
###################
## code to set up GED/ACLED dataframe on the province-month level
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

# ## taken from click click boom
# countries <- c("Algeria",
#                "Angola",
#                "Benin",
#                "Botswana",
#                "Burkina_Faso",
#                "Burundi",
#                "Cameroon",
#                # "Cape_Verde",
#                "Central_African_Republic",
#                "Chad",
#                # "Comoros",
#                "Democratic_Republic_of_the_Congo",
#                "Republic_of_the_Congo",
#                "Ivory_Coast",
#                "Djibouti",
#                "Egypt",
#                "Equatorial_Guinea",
#                "Eritrea",
#                "Eswatini",
#                "Ethiopia",
#                "Gabon",
#                "The_Gambia",
#                "Ghana",
#                "Guinea",
#                "Guinea-Bissau",
#                "Kenya",
#                "Lesotho",
#                "Liberia",
#                "Libya",
#                "Madagascar",
#                "Malawi",
#                "Mali",
#                "Mauritania",
#                # "Mauritius",
#                "Morocco",
#                "Mozambique",
#                "Namibia",
#                "Niger",
#                "Nigeria",
#                "Rwanda",
#                # "São_Tomé_and_Príncipe",
#                "Senegal",
#                # "Seychelles",
#                "Sierra_Leone",
#                "Somalia",
#                "South_Africa",
#                "South_Sudan",
#                "Sudan",
#                # "Swaziland",
#                "Tanzania",
#                "Togo",
#                "Tunisia",
#                "Uganda",
#                "Zambia",
#                "Zimbabwe") %>%
#   str_replace_all("_", " ") #%>%
# #   tolower()
# ## make gwno list for filtering ged events later
# ## sao tome principe and seychelles not matched unambiguously (drop?)
# ## gwn: gleditsch ward numeric, gwc: gleditsch ward character
# gwno.codes <- countrycode::countrycode(countries, origin = "country.name", destination = "gwn")
# 
# ## custom dictionary for adding country names later (ISO codes do not run smoothly via countryname()..)
# country_dict <- data.frame(matrix(c("Algeria", "DZA",
#                                     "Angola", "AGO",
#                                     "Benin", "BEN",
#                                     "Botswana", "BWA",
#                                     "Burkina Faso", "BFA",
#                                     "Burundi", "BDI",
#                                     "Cameroon", "CMR",
#                                     # "Cape Verde", "CPV",
#                                     "Central African Republic", "CAF",
#                                     "Chad", "TCD",
#                                     # "Comoros", "COM",
#                                     "Democratic Republic of the Congo", "COD",
#                                     "Republic of the Congo", "COG",
#                                     "Ivory Coast", "CIV",
#                                     "Djibouti", "DJI",
#                                     "Egypt", "EGY",
#                                     "Equatorial Guinea", "GNQ",
#                                     "Eritrea", "ERI",
#                                     "Eswatini", "SWZ",
#                                     "Ethiopia", "ETH",
#                                     "Gabon", "GAB",
#                                     "The Gambia", "GMB",
#                                     "Ghana", "GHA",
#                                     "Guinea", "GIN",
#                                     "Guinea-Bissau", "GNB",
#                                     "Kenya", "KEN",
#                                     "Lesotho", "LSO",
#                                     "Liberia", "LBR",
#                                     "Libya", "LBY",
#                                     "Madagascar", "MDG",
#                                     "Malawi", "MWI",
#                                     "Mali", "MLI",
#                                     "Mauritania", "MRT",
#                                     # "Mauritius", "MUS",
#                                     "Morocco", "MAR",
#                                     "Mozambique", "MOZ",
#                                     "Namibia", "NAM",
#                                     "Niger", "NER",
#                                     "Nigeria", "NGA",
#                                     "Rwanda", "RWA",
#                                     # "São_Tomé_and_Príncipe", "STP",
#                                     "Senegal", "SEN",
#                                     # "Seychelles", "SYC",
#                                     "Sierra Leone", "SLE",
#                                     "Somalia", "SOM",
#                                     "South Africa", "ZAF",
#                                     "South Sudan", "SSD",
#                                     "Sudan", "SDN",
#                                     "Tanzania", "TZA",
#                                     "Togo", "TGO",
#                                     "Tunisia", "TUN",
#                                     "Uganda", "UGA",
#                                     "Zambia", "ZMB",
#                                     "Zimbabwe", "ZWE"),
#                                   ## remember to adjust nrow when adding / deleting countries
#                                   nrow = 49, ncol = 2, byrow = TRUE, 
#                                   dimnames = list(NULL, c("country_name", "ISO"))))
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


## read in africa country list
countries_africa_adm1 <- readRDS("rds/lists/africa_dict_filled.rds") %>%
  ## replace underscores from wikipedia page names
  mutate_all(~ str_replace_all(., "_", " ")) %>%
  ## replace commas in names
  mutate_all(~ str_replace_all(., ", ", " ")) %>%
  mutate_all(~ str_replace_all(., "/", "%E2%88%95F")) %>%
  ## replace brackets and all info in it (e.g. "(Seychelles)")
  mutate_all(~ str_replace_all(., " \\s*\\([^\\)]+\\)", "")) %>%
  ## replace this utf encoding which wasn't caught with the utf encode function
  mutate_all(~ str_replace_all(., "%27", "'"))

countries_africa_adm1$isocode2full <- str_replace_all(countries_africa_adm1$isocode2full, " ", "_")
sum(is.na(countries_africa_adm1))


## for merging with ged and acled data later
gadmprep <- readRDS("data/gadm/gadmafrica.rds") %>%
  rename(
    isocode1 = GID_0,
    isoname1 = NAME_0,
    isocode2full = GID_1,
    isocode3full = GID_2,
    isoname2 = NAME_1,
    altname2 = VARNAME_1,
    nalname2 = NL_NAME_1,
    isoname3 = NAME_2,
    altname3 = VARNAME_2,
    nalname3 = NL_NAME_2) %>%
  select(
    isocode1, isoname1,isocode2full, isoname2, isocode3full, isoname3, altname3, nalname3) %>%
  as.data.frame() %>%
  filter(isocode1 %in% country_dict$ISO) %>%
  ## extract the gadm id for the first and second subnational unit to merge with xsub later
  mutate(
    isocode2 = as.numeric(gsub(".*\\.(\\d+)_.*", "\\1", isocode2full)),
    isocode3 = str_replace(str_sub(isocode3full, 7, 8), "_", "")) %>%
  select(-c(isocode3full, isoname3, isocode3, altname3, nalname3)) %>%
  unique() %>%
  sf::st_as_sf()
gadmprep$isocode2[gadmprep$isocode2full == "GHA1_2"] <- 1
gadmprep$isocode2[gadmprep$isocode2full == "GHA2_2"] <- 2
gadmprep$isocode2[gadmprep$isocode2full == "GHA3_2"] <- 3
gadmprep$isocode2[gadmprep$isocode2full == "GHA4_2"] <- 4
gadmprep$isocode2[gadmprep$isocode2full == "GHA5_2"] <- 5
gadmprep$isocode2[gadmprep$isocode2full == "GHA6_2"] <- 6
gadmprep$isocode2[gadmprep$isocode2full == "GHA7_2"] <- 7
gadmprep$isocode2[gadmprep$isocode2full == "GHA8_2"] <- 8
gadmprep$isocode2[gadmprep$isocode2full == "GHA9_2"] <- 9
gadmprep$isocode2[gadmprep$isocode2full == "GHA10_2"] <- 10
gadmprep$isocode2[gadmprep$isocode2full == "GHA11_2"] <- 11
gadmprep$isocode2[gadmprep$isocode2full == "GHA12_2"] <- 12
gadmprep$isocode2[gadmprep$isocode2full == "GHA13_2"] <- 13
gadmprep$isocode2[gadmprep$isocode2full == "GHA14_2"] <- 14
gadmprep$isocode2[gadmprep$isocode2full == "GHA15_2"] <- 15
gadmprep$isocode2[gadmprep$isocode2full == "GHA16_2"] <- 16

sum(is.na(gadmprep))



basedf <- expand.grid(isocode2full = unique(countries_africa_adm1$isocode2full), 
                      year = years,
                      month = months) %>%
  left_join(countries_africa_adm1) %>%
  select(-starts_with("wikipedia")) %>%
  mutate(isocode2 = as.integer(isocode2))
sum(is.na(basedf)) ## 0
sapply(basedf, function(x) sum(is.na(x)))


ged_adm1_africa_frame <- NULL

## data sources to loop over
datasources <- c("ACLED", "GED", "PITF", "SCAD")


for (i in datasources) {
  print(i)
  frame_xsub <- get_xSub_multi(data_source = c(i),
                               ## 6,976 obs
                               country_iso3 = country_dict$ISO,
                               space_unit = "adm1",
                               time_unit = "month") %>%
    ## prio identifiers
    rename(isocode2 = ID_1,
           isocode1 = ISO,
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
           road_density = ROAD_DENSITY,
           temp = TEMP,
           rain = RAIN,
           # all the below are for prio grid and have many missing values (pop sum 4834, gdp 3322, nl 2902)
           ## linearly interpolated for NA
           pop_sum = PG_POP_HYD_SUM_LI,
           ## linearly interpolated for NA
           gdp_ppp = PG_GCP_PPP_LI,
           ## linearly interpolated for NA
           nl = PG_NLIGHTS_CALIB_MEAN_LI
    ) %>%
    select(isocode1, isocode2, 
           year, yearmonth,
           elev_mean, open_terrain, forest, farmland, nethgr, nbuiltup, npetro, 
           temp, rain, pop_sum, gdp_ppp, nl,
           road_length, road_density) %>%
    filter(year %in% years) %>%
    unique() %>%
    mutate(month = lubridate::month(lubridate::ym(yearmonth))) %>%
    select(-yearmonth)

  if (is.null(ged_adm1_africa_frame)) {
    ged_adm1_africa_frame <- left_join(basedf, frame_xsub, by = c("year", "month", "isocode1", "isocode2")) %>% unique()
    print("initialize basedf acled")
  } else {
    ged_adm1_africa_frame <- left_join(ged_adm1_africa_frame, frame_xsub, by = c("year", "month", 
                                                                                 "isocode1", "isocode2")) %>% 
      mutate(elev_mean = coalesce(elev_mean.x, elev_mean.y),
             open_terrain = coalesce(open_terrain.x, open_terrain.y),
             forest = coalesce(forest.x, forest.y),
             farmland = coalesce(farmland.x, farmland.y),
             nethgr = coalesce(nethgr.x, nethgr.y),
             nbuiltup = coalesce(nbuiltup.x, nbuiltup.y),
             npetro = coalesce(npetro.x, npetro.y),
             road_length = coalesce(road_length.x, road_length.y),
             road_density = coalesce(road_density.x, road_density.y),
             temp = coalesce(temp.x, temp.y,),
             rain = coalesce(rain.x, rain.y),
             pop_sum = coalesce(pop_sum.x, pop_sum.y),
             gdp_ppp = coalesce(gdp_ppp.x, gdp_ppp.y),
             nl = coalesce(nl.x, nl.y)
      ) %>%
      select(-c(elev_mean.x, elev_mean.y,
                open_terrain.x, open_terrain.y,
                forest.x, forest.y,
                farmland.x, farmland.y,
                nethgr.x, nethgr.y,
                nbuiltup.x, nbuiltup.y,
                npetro.x, npetro.y,
                road_length.x, road_length.y,
                road_density.x, road_density.y,
                temp.x, temp.y,
                rain.x, rain.y,
                pop_sum.x, pop_sum.y,
                gdp_ppp.x, gdp_ppp.y,
                nl.x, nl.y)
             ) %>%
      unique()
    print("maintain basedf acled")
  }
}

## add cow and gwn for merging data later
ged_adm1_africa_frame <- ged_adm1_africa_frame %>%
  mutate(cow = countrycode::countrycode(isocode1, origin = "iso3c", destination = "cown"),
         gwno = countrycode::countrycode(isocode1, origin = "iso3c", destination = "gwn",
                                         ## sao tome/principe & seychelles
                                         custom_match = c(c("STP" = 403),
                                                          c("SYC" = 591))))


ls(ged_adm1_africa_frame)
sum(is.na(ged_adm1_africa_frame)) ## 998515
sapply(ged_adm1_africa_frame, function(x) sum(is.na(x)))

length(unique(frame_xsub$isocode1))
length(unique(frame_xsub$isocode2))

length(unique(ged_adm1_africa_frame$isocode1)) ## 49 unique countries

ged_adm1_africa_frame <- ged_adm1_africa_frame %>%
  arrange(year, month) %>%
  group_by(isocode2full) %>%
  fill(elev_mean, .direction = "updown") %>%
  fill(open_terrain, .direction = "updown") %>%
  fill(forest, .direction = "updown") %>%
  fill(farmland, .direction = "updown") %>%
  fill(nethgr, .direction = "updown") %>%
  fill(nbuiltup, .direction = "updown") %>%
  fill(npetro, .direction = "updown") %>%
  fill(road_length, .direction = "updown") %>%
  fill(road_density, .direction = "updown") %>%
  fill(temp, .direction = "updown") %>%
  fill(rain, .direction = "updown") %>%
  fill(pop_sum, .direction = "updown") %>%
  fill(gdp_ppp, .direction = "updown") %>%
  fill(nl, .direction = "updown") %>%
  ungroup()

sum(is.na(ged_adm1_africa_frame)) ## 257856
sapply(ged_adm1_africa_frame, function(x) sum(is.na(x)))


saveRDS(ged_adm1_africa_frame, "rds/imputations/ged_adm1_africa_frame.rds")

## impute missing values
## this took a very long time and was done in a separate script on a high performance computer (hpc)
## about 25 hours on hpc
ged_adm1_africa_frame <- readRDS("rds/imputations/ged_adm1_africa_frame.rds")

library(parallel)
library(doParallel)

cores <- 48
cl <- makeCluster(cores) ## convention to leave 1 core for OS
registerDoParallel(cl)

library(mice)
M <- 100

start1 <- Sys.time()
gedframe_cart <- mice(ged_adm1_africa_frame, m=M, seed=0815, method='cart')
end1 <- Sys.time()
saveRDS(gedframe_cart, "rds/imputations/gedframe_cart.rds")

end1-start1 ## 12-14 min

stopCluster(cl)
registerDoSEQ()

## end impute missing values (move back to local machine)
gedframe_cart <- readRDS("rds/imputations/gedframe_cart.rds")

sum(is.na(gedframe_cart)) ## 0
gedframe_cart2 <- complete(gedframe_cart)
sum(is.na(gedframe_cart2))
sapply(gedframe_cart2, function(x) sum(is.na(x)))

gedframe_cart2$isocode2full <- str_replace_all(gedframe_cart2$isocode2full, " ", "_")

basedf_full <- left_join(basedf, gedframe_cart2) %>%
  unique()

sum(is.na(basedf_full)) ## 0
length(unique(basedf_full$isocode1))
rm(ged_adm1_africa_frame)



gwno.codes <- unique(basedf_full$gwno)
## bring in peacesciencer data here
library(peacesciencer)
peacesciencer_frame <- create_statedays(system = 'gw') %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  distinct(gwcode, statename, year, month) %>%
  filter(year >= min(years)) %>%
  dplyr::filter(gwcode %in% basedf_full$gwno)

acdframe <- create_stateyears(system = 'gw') %>%
  dplyr::filter(gwcode %in% basedf_full$gwno) %>%
  ## add cowcode for merging later
  ## var name ccode
  add_ccode_to_gw() %>%
  ## add ucdp onset info ()
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
  #  add_ucdp_acd(type=c("intrastate"), only_wars = FALSE) #%>%
  ## add int gov organizations data from cow
  ## var names: sum igo anytype, sum igo associate, sum igo full, sum igo observer
  ## These are the number of IGOs for which the state is a full member, the number of IGOs for which the state
  ## is an associate member, the number of IGOs for which the state is an observer, and the number of IGOs for
  ## which the state is involved in any way (i.e. the sum of the other three columns).
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
  # filter(year %in% c(1981:2010)) %>%
  filter(year >= min(years)) %>%
  ## remove unnecessary variables
  ## also leave out sdpest, too many NAs which cannot be imputed it seems, gdp is enough
  select(-c(dec31obsid, jan1obsid, 
            #polity2, sdpest,
            sumnewconf, sumonset1, sumonset2, sumonset3, sumonset5, sumonset10,
            maxintensity, conflict_ids, ccode))

range(acdframe$year) ## 2008 - 2023
sum(is.na(acdframe)) ## 10358
sapply(acdframe, function(x) sum(is.na(x)))


## impute missing values (move over to hpc since it takes ages)
library(mice)

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
  mutate_at(vars(
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
sapply(peacesciencer_full, function(x) sum(is.na(x)))

## remove unnecessary objects
rm(peacesciencer_frame)
rm(acdframe)
rm(acdframe2)
rm(acdframe_cart)
rm(acdframe_cart2)


## merge basedf and peacesciencer data
basedf_full2 <- left_join(basedf_full, peacesciencer_full)

sum(is.na(basedf_full2))
sapply(basedf_full2, function(x) sum(is.na(x)))
missings_tab <- basedf_full2[rowSums(is.na(basedf_full2)) > 0, ]

naniar::gg_miss_case(basedf_full2, facet = year) + labs(x = "Number of cases")


## load and prepare GED data

## ged curated annual release data
ged.raw <- read.csv("data/events/GEDEvent_v23_1.csv")
## ged candidate monthly release data
ged.cand.raw <- read.csv("data/events/GEDEvent_v23_01_23_12.csv")


## create state-based violence dataframe
ged.sbv <- ged.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
  summarise(sbv_fat_be = sum(best),
            sbv_fat_lo = sum(low),
            sbv_fat_hi = sum(high),
            sbv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.sbv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
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
  group_by(isocode2full) %>%
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
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
  summarise(osv_fat_be = sum(best),
            osv_fat_lo = sum(low),
            osv_fat_hi = sum(high),
            osv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.osv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
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
  group_by(isocode2full) %>%
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
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
  summarise(nsv_fat_be = sum(best),
            nsv_fat_lo = sum(low),
            nsv_fat_hi = sum(high),
            nsv_fat_no = n()
  ) %>%
  ungroup() %>%
  unique()


ged.cand.nsv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= min(years)) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
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
                longitude,
                latitude,
                best,
                low,
                high,
                eventid = id) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
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
  group_by(isocode2full) %>%
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

backup <- basedf_full
sbv.backup <- ged.sbv.full
osv.backup <- ged.osv.full
nsv.backup <- ged.nsv.full

sum(is.na(ged.sbv.full)) ## 110
sum(is.na(ged.osv.full)) ## 33
sum(is.na(ged.nsv.full)) ## 4
ged.sbv.full <- na.omit(ged.sbv.full) %>% as.data.frame() %>% select(-geometry)
ged.osv.full <- na.omit(ged.osv.full) %>% as.data.frame() %>% select(-geometry)
ged.nsv.full <- na.omit(ged.nsv.full) %>% as.data.frame() %>% select(-geometry)

basedf_full <- left_join(basedf_full, ged.sbv.full)
basedf_full <- left_join(basedf_full, ged.osv.full)
basedf_full <- left_join(basedf_full, ged.nsv.full)
sum(is.na(basedf_full))
sapply(basedf_full, function(x) sum(is.na(x)))

## acled october 21 release data
acled.raw <- read.csv("data/events/1997-01-01-2024-05-01.csv")


acled.sri.full <- acled.raw %>%
  ## rename a few variables for later
  dplyr::mutate(date = lubridate::dmy(event_date),
                ## three character iso code
                ISO = iso,
                gwno = countrycode::countrycode(ISO, origin = "iso3n", destination = "gwn"),
                month = lubridate::month(date)) %>%
  ## only data starting in 2008 of interest
  filter(year >= min(years)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_full$gwno) %>%
  ## filter security-related incidents
  dplyr::filter(event_type %in% c("Battles", "Explosions/Remote violence", "Violence against civilians")
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                country,
                gwno,
                longitude,
                latitude,
                event_type,
                fatalities,
                admin1
  ) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84") %>%
  sf::st_join(gadmprep) %>%
  ## use newly added isocode2full for easier merging later
  group_by(isocode2full, year, month) %>%
  summarise(sri_fat = sum(fatalities),
            sri_num = n()
  ) %>%
  ungroup() %>%
  unique() %>%
  mutate(sri_fat_log = log1p(sri_fat),
         sri_num_log = log1p(sri_num)) %>%
  arrange(year, month) %>%
  group_by(isocode2full) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
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
sum(is.na(acled.sri.full)) ## 183
sapply(acled.sri.full, function(x) sum(is.na(x)))
acled.sri.full <- na.omit(acled.sri.full) %>% as.data.frame() %>% select(-geometry)

basedf_full <- left_join(basedf_full, acled.sri.full)
sum(is.na(basedf_full2)) ## 183
sapply(basedf_full2, function(x) sum(is.na(x)))
basedf_full <- basedf_full %>% replace(is.na(.), 0)
sum(is.na(basedf_full2)) ## 0

saveRDS(basedf_full, "data/basedf_full_adm1_afr.rds")



## clear environment
rm(list = ls())

## load libraries
library(tidyverse)

## combine these with gtrends and wikipedia data
basedf_full <- readRDS("data/basedf_full_adm1_afr.rds") %>%
  ## leaving it would cause a merging conflict below with gtwikidata
  select(-gadmname)
sum(is.na(basedf_full))
sapply(basedf_full, function(x) sum(is.na(x)))


page_views_adm1_afr_all_lang_final <- readRDS("data/wikipedia/new/pageviews_admin1_africa_all_lang_final_20221219.rds")
googletrends_onelangallgprops <- readRDS("data/googletrends/gtrends_admin1_all_lang.rds")

sum(is.na(page_views_adm1_afr_all_lang_final))
sum(is.na(googletrends_onelangallgprops))
ls(page_views_adm1_afr_all_lang_final)
ls(googletrends_onelangallgprops)

str(page_views_adm1_afr_all_lang_final$isocode2full)
str(googletrends_onelangallgprops$isocode2full)
str(page_views_adm1_afr_all_lang_final$year)
str(googletrends_onelangallgprops$year)
str(page_views_adm1_afr_all_lang_final$month)
str(googletrends_onelangallgprops$month)

gtwikidata <- left_join(page_views_adm1_afr_all_lang_final, googletrends_onelangallgprops,
                        by = c("isocode2full", "year", "month")) %>% 
  unique() %>%
  mutate(gadmname = coalesce(gadmname.x, gadmname.y),
         country = coalesce(country.x, country.y)) %>%
  select(-c(gadmname.x, gadmname.y, country.x, country.y))

sum(is.na(gtwikidata))
sapply(gtwikidata, function(x) sum(is.na(x)))
missings_tab <- gtwikidata[rowSums(is.na(gtwikidata)) > 0, ]

gtwikidata_africa_admin1 <- gtwikidata %>%
  select(-(starts_with("wikipedia")))

ls(gtwikidata)

ls(basedf_full)
str(basedf_full$isocode2full)
str(gtwikidata$isocode2full)
str(basedf_full$year)
str(gtwikidata$year)
str(basedf_full$month)
str(gtwikidata$month)
fulldata <- left_join(basedf_full, gtwikidata)
sum(is.na(fulldata)) ## 0
sapply(fulldata, function(x) sum(is.na(x)))

saveRDS(fulldata, "data/fulldata_adm1_africa.rds")
