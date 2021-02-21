# get a random sample of 100 mothers from IDI Pilot--------

# libraries and helper files
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(plotly)
library(expss)
source("checks.R")
source("helper.R")

# Keep IDI data
teamIDI <- c("Mahendra", "Pradeep kumar mishra", "Rajan singh", "Suchita tiwari", "Sangeeta Devi", "BRAHM DEV VERMA", "Sonu verma")
ds_wide_expand_idi <- dplyr::filter(ds_wide_expand, member_name %in% teamIDI)

# select 100 random mothers from IDI data
numMothers <- 100
ds_wide_expand_idi_rand <- sample_n(ds_wide_expand_idi, numMothers)

# further select relevant columns to export as prefill dataset
ds_wide_expand_idi_rand_rel_col <- dplyr::select(ds_wide_expand_idi_rand, SubmissionDate, KEY, mothers_name, mothers_contact_number,
                                                 fathers_name, fathers_contact_number,
                                                 eligibility_check_pregnant, eligibility_check_child, child1s_name)


# get alternate contact information for specific KEY's

keyVec <- c("uuid:f5750d34-98f5-4047-9cd5-9e63dac63043", 
            "uuid:437c2531-d6ae-4189-9e62-7194c992b39b",
            "uuid:0dc84607-d7f8-4b19-ae69-f0a68b9ee5c7", 
            "uuid:02210cb8-9882-45b9-a2d1-015b65e0a7a7", 
            "uuid:a588fef6-1d40-4d10-ade9-98a04f8f80e9",
            "uuid:b956e30b-efea-44e0-8e7e-3eb4a80d2432",
            "uuid:386c9a6e-a64e-4b3c-baf6-201346991370",
            "uuid:ba1b016f-d5f2-4c6a-b7ed-99a903841a22",
            "uuid:abc09199-1524-4fe6-9158-ffdc64138493")

ds_wide_expand_keyVec <- dplyr::filter(ds_wide_expand, KEY %in% keyVec)
ds_wide_expand_keyVec_altCont <- dplyr::select(ds_wide_expand_keyVec, KEY, SubmissionDate, alternate_contact_1_name, alternate_contact1_number, 
                                               alternate_contact_2_name, alternate_contact2_contact_number, maternalHome_contact_number)


# generate a quick distribution of the number of vaccination visits for child1 given that child 1 is older than 6 months-------
ds_wide_expand$child1s_dob <- lubridate::mdy(ds_wide_expand$child1s_dob)
ds_wide_expand_chOldThan6Months_nonNA <- dplyr::filter(ds_wide_expand, ((today() - child1s_dob) >= 180), !is.na(child1_number_of_vacc_visits_recall), !is.na(child1s_dob))
plt <- ggplot(data = ds_wide_expand_chOldThan6Months_nonNA) + geom_histogram(mapping = aes(x = child1_number_of_vacc_visits_recall))
ggplotly(plt)

