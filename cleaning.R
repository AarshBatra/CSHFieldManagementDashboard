## cleaning file: datamation survey---------

## meta data---------
# Author: Aarsh Batra
# Started on: March 30, 2019
# Purpose: Generate varibales from the cleaned dataset.

## set user-----------

## setwd----------
# setwd("C:/Users/Aarsh/Dropbox/CSH/FieldManagementDashboard")

## helper files----------
source("helper.R")

## WIDE data from Survey CTO---------
ds_wide_raw <- read_csv("Enrollment survey D2D partners 20190223_WIDE.csv")
ds_wide_colnames <- colnames(ds_wide_raw)

# types of columns: subsets-----------
ds_wide_loc_col <- str_get(ds_wide_colnames, "location") # gps columns
ds_wide_ch1_col <- str_get(ds_wide_colnames, "child1")
ds_wide_ch2_col <- str_get(ds_wide_colnames, "child2")

# clean the dataset
ds_wide_clean <- ds_wide_raw





