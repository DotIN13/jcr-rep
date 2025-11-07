###################
###################
## code to set up GED/ACLED dataframe on the country-month level
## global
###################
###################

## clear environment
rm(list = ls())

## load libraries
library(tidyverse)
library(xSub)


years <- seq(2008, 2023, 1)
months <- seq(1, 12, 1)

## countries taken from https://en.wikipedia.org/wiki/List_of_sovereign_states
countries <- c("Afghanistan",
               "Albania",
               "Algeria",
               "Andorra",
               "Angola",
               "Antigua_and_Barbuda",
               "Argentina",
               "Armenia",
               "Australia",
               "Austria",
               "Azerbaijan",
               "The_Bahamas",
               "Bahrain",
               "Bangladesh",
               "Barbados",
               "Belarus",
               "Belgium",
               "Belize",
               "Benin",
               "Bhutan",
               "Bolivia",
               "Bosnia_and_Herzegovina",
               "Botswana",
               "Brazil",
               "Brunei",
               "Bulgaria",
               "Burkina_Faso",
               "Burundi",
               "Cambodia",
               "Cameroon",
               "Canada",
               "Cape_Verde",
               "Central_African_Republic",
               "Chad",
               "Chile",
               "China",
               "Colombia",
               "Comoros",
               "Democratic_Republic_of_the_Congo",
               "Republic_of_the_Congo",
               "Costa_Rica",
               "Croatia",
               "Cuba",
               "Cyprus",
               "Czech_Republic",
               "Denmark",
               "Djibouti",
               "Dominica",
               "Dominican_Republic",
               "East Timor",
               "Ecuador",
               "Egypt",
               "El_Salvador",
               "Equatorial_Guinea",
               "Eritrea",
               "Estonia",
               "Eswatini",
               "Ethiopia",
               "Fiji",
               "Finland",
               "France",
               "Gabon",
               "The_Gambia",
               "Georgia",
               "Germany",
               "Ghana",
               "Greece",
               "Grenada",
               "Guatemala",
               "Guinea",
               "Guinea-Bissau",
               "Guyana",
               "Haiti",
               "Honduras",
               "Hungary",
               "Iceland",
               "India",
               "Indonesia",
               "Iran",
               "Iraq",
               "Ireland",
               "Israel",
               "Italy",
               "Ivory_Coast",
               "Jamaica",
               "Japan",
               "Jordan",
               "Kazakhstan",
               "Kenya",
               "Kiribati",
               "North_Korea",
               "South_Korea",
               "Kuwait",
               "Kyrgyzstan",
               "Laos",
               "Latvia",
               "Lebanon",
               "Lesotho",
               "Liberia",
               "Libya",
               "Liechtenstein",
               "Lithuania",
               "Luxembourg",
               "Madagascar",
               "Malawi",
               "Malaysia",
               "Maldives",
               "Mali",
               "Malta",
               "Marshall_Islands",
               "Mauritania",
               "Mauritius",
               "Mexico",
               "Federated_States_of_Micronesia",
               "Moldova",
               "Monaco",
               "Mongolia",
               "Montenegro",
               "Morocco",
               "Mozambique",
               "Myanmar",
               "Namibia",
               "Nauru",
               "Nepal",
               "Netherlands",
               "New_Zealand",
               "Nicaragua",
               "Niger",
               "Nigeria",
               "North_Macedonia",
               "Norway",
               "Oman",
               "Pakistan",
               "Palau",
               "Panama",
               "Papua_New_Guinea",
               "Paraguay",
               "Peru",
               "Philippines",
               "Poland",
               "Portugal",
               "Qatar",
               "Romania",
               "Russia",
               "Rwanda",
               "Saint_Kitts_and_Nevis",
               "Saint_Lucia",
               "Saint_Vincent_and_the_Grenadines",
               "Samoa",
               "San_Marino",
               "São_Tomé_and_Príncipe",
               "Saudi_Arabia",
               "Senegal",
               "Serbia",
               "Seychelles",
               "Sierra_Leone",
               "Singapore",
               "Slovakia",
               "Slovenia",
               "Solomon_Islands",
               "Somalia",
               "South_Africa",
               "South_Sudan",
               "Spain",
               "Sri_Lanka",
               "Sudan",
               "Suriname",
               "Sweden",
               "Switzerland",
               "Swaziland",
               "Syria",
               "Taiwan",
               "Tajikistan",
               "Tanzania",
               "Thailand",
               "Togo",
               "Tonga",
               "Trinidad_and_Tobago",
               "Tunisia",
               "Turkey",
               "Turkmenistan",
               "Tuvalu",
               "Uganda",
               "Ukraine",
               "United_Arab_Emirates",
               "United_Kingdom",
               "United_States",
               "Uruguay",
               "Uzbekistan",
               "Vanuatu",
               "Venezuela",
               "Vietnam",
               "Yemen",
               "Zambia",
               "Zimbabwe") %>%
str_replace_all("_", " ")


basedf_global <- expand.grid(country_name = countries,
                      year = years,
                      month = months) %>%
  mutate(iso = countrycode::countrycode(country_name, origin = "country.name", destination = "iso3c"),
         gwno = as.integer(countrycode::countrycode(country_name, origin = "country.name", destination = "gwn", 
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
                                                          c("Yemen" = "678"))))
  )

## create year-month variable for plotting
basedf_global$yearmonth <- as.Date(paste(basedf_global$month, "01", basedf_global$year, sep = "_"), format = "%m_%d_%Y")

sum(is.na(basedf_global)) ## 0




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
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter state-based violence: government vs rebel group(s)
  dplyr::filter(type_of_violence == 1,
                # where_prec %in% c(1, 2, 3)
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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

## leaves 30 countries in africa with state-based violent events, sounds about right
# length(unique(ged.sbv$gwno))

ged.cand.sbv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter state-based violence: government vs rebel group(s)
  dplyr::filter(type_of_violence == 1,
                # where_prec %in% c(1, 2, 3)
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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
  unique() %>%
  mutate(sbv_fat_be_log = log1p(sbv_fat_be),
         sbv_fat_lo_log = log1p(sbv_fat_lo),
         sbv_fat_hi_log = log1p(sbv_fat_hi),
         sbv_fat_no_log = log1p(sbv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(## used elsewhere: order_by = timeindex; think about what to order by here (if at all)
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
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 3,
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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

## leaves 30 countries in africa with state-based violent events, sounds about right
# length(unique(ged.sbv$gwno))

ged.cand.osv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 3,
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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
  unique() %>%
  ## necessary since one country-month appears twice for some reason
  group_by(gwno, year, month) %>%
  summarise(osv_fat_be = sum(osv_fat_be),
            osv_fat_lo = sum(osv_fat_lo),
            osv_fat_hi = sum(osv_fat_hi),
            osv_fat_no = sum(osv_fat_no)) %>%
  ungroup() %>%
  mutate(osv_fat_be_log = log1p(osv_fat_be),
         osv_fat_lo_log = log1p(osv_fat_lo),
         osv_fat_hi_log = log1p(osv_fat_hi),
         osv_fat_no_log = log1p(osv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(## used elsewhere: order_by = timeindex; think about what to order by here (if at all)
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
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 2,
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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

## leaves 30 countries in africa with state-based violent events, sounds about right
# length(unique(ged.sbv$gwno))

ged.cand.nsv <- ged.cand.raw %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## rename a few variables for later
  dplyr::mutate(date = as.Date(date_start),
                # date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                gwno = country_id,
                day = lubridate::day(date_start),
                month = lubridate::month(date_start),
                date_end = replace(date_end, date_end == date_start, NA)) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global$gwno) %>%
  ## filter one-sided violence: government or rebel group(s) vs civilians
  dplyr::filter(type_of_violence == 2,
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                day,
                date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                gwno,
                region,
                adm_1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                best,
                low,
                high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
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
  unique() %>%
  mutate(nsv_fat_be_log = log1p(nsv_fat_be),
         nsv_fat_lo_log = log1p(nsv_fat_lo),
         nsv_fat_hi_log = log1p(nsv_fat_hi),
         nsv_fat_no_log = log1p(nsv_fat_no)) %>%
  arrange(year, month) %>%
  group_by(gwno) %>%
  ## create additional variables
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(## used elsewhere: order_by = timeindex; think about what to order by here (if at all)
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

basedf_global_full <- left_join(basedf_global, ged.sbv.full) %>% unique()
## ged.osv.full has one entry which doesnt seem to match perfectly
basedf_global_full <- left_join(basedf_global_full, ged.osv.full) %>% unique()
basedf_global_full <- left_join(basedf_global_full, ged.nsv.full) %>% unique()

## acled october 21 release data
# acled.raw <- readxl::read_excel("data/events/Africa_1997-2022_Oct21.xlsx", sheet = "Sheet1")
acled.raw <- read.csv("data/events/1997-01-01-2024-05-01.csv")


acled.sri.full <- acled.raw %>%
  ## rename a few variables for later
  dplyr::mutate(date = lubridate::dmy(event_date),
                ## three character iso code (iso3)
                ## number iso code (iso)
                ## variable name changed from iso3 to iso
                ISO = iso,
                # gwno = countrycode::countrycode(ISO, origin = "iso3c", destination = "gwn"),
                gwno = as.integer(countrycode::countrycode(country, origin = "country.name", destination = "gwn", 
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
                                                                            c("Palestine" = "666"),
                                                                            c("Saint Kitts and Nevis" = "60"),
                                                                            c("Saint Lucia" = "56"),
                                                                            c("Saint Vincent and the Grenadines" = "57"),
                                                                            c("Samoa" = "990"),
                                                                            c("San Marino" = "331"),
                                                                            c("São Tomé and Príncipe" = "403"),
                                                                            c("Sao Tome and Principe" = "403"),
                                                                            c("Seychelles" = "591"),
                                                                            c("Tonga" = "972"),
                                                                            c("Tuvalu" = "973"),
                                                                            c("Vanuatu" = "935"),
                                                                            c("Yemen" = "678")))),# date_end = lubridate::ymd(date_end),
                # extended = ifelse(date_start != date_end, 1, 0),
                # duration = date_end-date_start+1,
                # day = lubridate::day(event_date),
                month = lubridate::month(date)) %>%
  ## only data starting in 2008 of interest
  filter(year >= 2008) %>%
  ## filter countries of interest specified above
  dplyr::filter(gwno %in% basedf_global_full$gwno) %>%
  # dplyr::filter(ISO %in% basedf_global_full$iso) %>%
  ## filter security-related incidents
  dplyr::filter(event_type %in% c("Battles", "Explosions/Remote violence", "Violence against civilians")
  ) %>%
  
  ## select relevant variables
  dplyr::select(year,
                month,
                # day,
                # date,
                # date_end,
                # date_prec,
                # extended,
                # duration,
                country,
                # iso, 
                gwno,
                # region,
                # admin1,
                # longitude,
                # latitude,
                # specificity = where_prec,
                event_type,
                fatalities
                # low,
                # high,
                # perpetrator = side_a,
                # target = side_b,
                # reb_deaths = deaths_b,
                # gov_deaths = deaths_a,
                # civ_deaths = deaths_civilians,
                # type_of_violence,
                # gid = priogrid_gid,
                # adm_1,
                # adm_2,
                # gwnoa,
                # gwnob,
  ) %>%
  ## group by country-year-month and summarize fatalities (best, low, high) and number of sbv events
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
  ## change in fatalities from one month to another
  ## change in log fatalities from one month to another
  mutate(## used elsewhere: order_by = timeindex; think about what to order by here (if at all)
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


unique(countrycode::countrycode(ged.sbv.full$gwno, origin = "gwn", destination = "country.name"))
rm(acled.raw)
sum(is.na(acled.sri.full)) ## 0

# ## basedf_global_full$iso is character
# acled.sri.full$iso <- as.character(acled.sri.full$iso)

basedf_global_full <- left_join(basedf_global_full, acled.sri.full) %>% unique()

sum(is.na(basedf_global_full)) ## 3169908
sapply(basedf_global_full, function(x) sum(is.na(x)))

## replace all NAs with 0s
basedf_global_full <- basedf_global_full %>% replace(is.na(.), 0)
sum(is.na(basedf_global_full)) ## 0

saveRDS(basedf_global_full, "data/basedf_global_full.rds")



## clear environment
rm(list = ls())

## load libraries
library(tidyverse)

## combine these with gtrends and wikipedia data
basedf_global_full <- readRDS("data/basedf_global_full.rds") %>%
  unique() %>%
  mutate(country_name = as.character(country_name))
# year = as.character(year),
# month = as.character(month))
## replace this later
## right now only wikipedia data in here
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


# gtwikidata_global <- readRDS("data/wikipedia/new/pageviews_global_country_all_lang.rds") %>% 
#   unique() %>%
#   mutate(month = as.numeric(month),
#          year = as.numeric(year),
#          gwno = as.integer(countrycode::countrycode(str_replace_all(country_name, "_", " "), origin = "country.name", destination = "gwn", 
#                                                     ## microstates not automatically matched
#                                                     ## create custom match
#                                                     ## http://ksgleditsch.com/data/microstatessystem.dat
#                                                     ## http://ksgleditsch.com/data/iisystem.dat
#                                                     custom_match = c(c("Andorra" = "232"),
#                                                                      c("Antigua and Barbuda" = "58"), 
#                                                                      c("Dominica" = "54"), 
#                                                                      c("Grenada" = "55"), 
#                                                                      c("Kiribati" = "970"),
#                                                                      c("Liechtenstein" = "223"),
#                                                                      c("Marshall Islands" = "983"),
#                                                                      c("Federated States of Micronesia" = "987"),
#                                                                      c("Monaco" = "221"),
#                                                                      c("Nauru" = "971"),
#                                                                      c("Palau" = "986"),
#                                                                      c("Saint Kitts and Nevis" = "60"),
#                                                                      c("Saint Lucia" = "56"),
#                                                                      c("Saint Vincent and the Grenadines" = "57"),
#                                                                      c("Samoa" = "990"),
#                                                                      c("San Marino" = "331"),
#                                                                      c("São Tomé and Príncipe" = "403"),
#                                                                      c("Seychelles" = "591"),
#                                                                      c("Tonga" = "972"),
#                                                                      c("Tuvalu" = "973"),
#                                                                      c("Vanuatu" = "935"),
#                                                                      c("Yemen" = "678")))))

sum(is.na(gtwikidata_global))
sapply(gtwikidata_global, function(x) sum(is.na(x)))

## wiki and gt data have "Georgia (country)"
## basedf has country name "Georgia"
gtwikidata_global$country_name[gtwikidata_global$country_name == "Georgia (country)"] <- "Georgia"
# arrow::write_parquet(gtwikidata_global, "gtwikidata_global_admin0.parquet")

ls(basedf_global_full)
str(basedf_global_full$country_name)
str(gtwikidata_global$country_name)
str(basedf_global_full$year)
str(gtwikidata_global$year)
str(basedf_global_full$month)
str(gtwikidata_global$month)
str(basedf_global_full$gwno)
str(gtwikidata_global$gwno)
## something about country names here

fulldata_global <- left_join(basedf_global_full, gtwikidata_global) ## , by = c("country_name", "gwno", "year", "month")

sum(is.na(fulldata_global)) ## 0
sapply(fulldata_global, function(x) sum(is.na(x)))

# ## replace all NAs with 0s
# fulldata <- fulldata %>% replace(is.na(.), 0)
# sum(is.na(fulldata))

saveRDS(fulldata_global, "data/fulldata_global.rds")
