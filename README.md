# google-wikipedia-prediction
Repository for the paper I still haven't found what I'm looking for

## Content
This repository contains all scripts used to generate and manipulate data, conduct analyses, produce results plots and tables, and descriptive statistics tables. The scripts are ordered and named numerically. 
It further contains rds files which contain the full data for the three types of analyses (Africa and world on country-level and Africa on province-level), i.e. the output after running scripts numbered 01 through 03, and two lists with country and province information. These files are sufficient to reproduce the Google Trends and Wikipedia data gathering scripts, and they are sufficient to run the data preparation scripts (04-) and all scripts following them. The analysis scripts are separated for computational and server/high performance computing infrastructures reasons. 

## General note
The imputation of the covariate data takes a very long time (>24h on a high performance computer using 48 cores for provinces in Africa). 
The random forests also take a long time to estimate (on average around 24h on a high performance computer using 48 cores). 
