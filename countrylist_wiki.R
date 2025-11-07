## clear environment
rm(list = ls())

## load libraries
library(wikipediatrend)
library(tidyverse)

## function to turn eg "T%C3%BAnez" into "Túnez"
## necessary so that looping through to get page views works later on
url_decode_utf = function(x) {
  y = urltools::url_decode(x)
  Encoding(y) = "UTF-8"
  y
}

## https://en.wikipedia.org/wiki/List_of_sovereign_states
## global
countries_global_wiki <- data.frame(matrix(c("Afghanistan",
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
                                 "East_Timor",
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
                                 "Georgia_(country)",
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
                                 "Zimbabwe"),
                               nrow = 195, ncol = 1, byrow = TRUE,
                               dimnames = list(NULL, "wikipedia_en")))


## add pages in other languages
for (i in 1:nrow(countries_global_wiki)) {
  print(countries_global_wiki$wikipedia_en[i])
  ## spanish (es)
  countries_global_wiki$wikipedia_es[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "es") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "es"]))
  # countries_global_wiki$wikipedia_es[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "es"]
  ## french )fr)
  countries_global_wiki$wikipedia_fr[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "fr") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "fr"]))
  # countries_global_wiki$wikipedia_fr[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "fr"]
  ## chinese (zh)
  countries_global_wiki$wikipedia_zh[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "zh") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "zh"]))
  # countries_global_wiki$wikipedia_zh[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "zh"]
  ## hindi (hi)
  countries_global_wiki$wikipedia_hi[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "hi") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "hi"]))
  # countries_global_wiki$wikipedia_hi[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "hi"]
  ## arabic (ar)
  countries_global_wiki$wikipedia_ar[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ar") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ar"]))
  # countries_global_wiki$wikipedia_ar[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ar"]
  ## bengali (bn)
  countries_global_wiki$wikipedia_bn[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "bn") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "bn"]))
  # countries_global_wiki$wikipedia_bn[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "bn"]
  ## urdu (ur)
  countries_global_wiki$wikipedia_ur[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ur") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ur"]))
  # countries_global_wiki$wikipedia_ur[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ur"]
  ## portuguese (pt)
  countries_global_wiki$wikipedia_pt[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "pt") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "pt"]))
  # countries_global_wiki$wikipedia_pt[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "pt"]
  ## german (de)
  countries_global_wiki$wikipedia_de[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "de") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "de"]))
  # countries_global_wiki$wikipedia_de[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "de"]
  ## russian (ru)
  countries_global_wiki$wikipedia_ru[i] <- url_decode_utf(ifelse((wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ru") == TRUE, 0, wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ru"]))
  # countries_global_wiki$wikipedia_ru[i] <- wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$page[wp_linked_pages(countries_global_wiki$wikipedia_en[i], "en")$lang == "ru"]
}
sum(is.na(countries_global_wiki))


saveRDS(countries_global_wiki, "rds/lists/countries_global_wiki.rds")

countries_global_wiki <- readRDS("rds/lists/countries_global_wiki.rds")


## africa
## subset from global list
## africa
africa <- c("Algeria",
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
            "São_Tomé_and_Príncipe",
            "Senegal",
            "Seychelles",
            "Sierra_Leone",
            "Somalia",
            "South_Africa",
            "South_Sudan",
            "Sudan",
            "Swaziland",
            "Tanzania",
            "Togo",
            "Tunisia",
            "Uganda",
            "Zambia",
            "Zimbabwe")

countries_africa_wiki <- countries_global_wiki %>%
  filter(wikipedia_en %in% africa)
sum(is.na(countries_africa_wiki))

saveRDS(countries_africa_wiki, "rds/lists/countries_wiki.rds")

