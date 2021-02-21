## getting prefill data for Follow up survey--------

# libraries---------
library(stringr)
library(readr)
library(tidyverse)
library(lubridate)

# helper files--------
source("checks.R")

# subsetting data to include only those people who signed up more than 2 months ago----------
# ds_wide_expand <- ds_wide_expand[ds_wide_expand$dateOnly < (today() - 60), ]

# get data after cleaning and generating variables and focussing only on child 1--------
follUpSurvRawData <- filter(ds_wide_expand, second_child_flag == 0)

# getting relevant columns for generation of prefill data---------
colNamesVec <- colnames(follUpSurvRawData)

# getting the columns that are to be pre-filled------------
relColNamesVec <- c("KEY", "SubmissionDate", "eligibility_check_pregnant", "eligibility_check_child", "mothers_name",
                    "mothers_contact_number", "mothers_preferred_language", "fathers_name", "fathers_contact_number",
                    "alternate_contact_1_name", "alternate_contact1_number", "alternate_contact_2_name",
                    "alternate_contact2_contact_number","child1s_name", "child1s_dob_vacc_card", "child1s_dob", "due_date_mcp_card",
                    "prenatal_receipt_due_date", "due_date_recall", "child1s_vacc_card_photo_consent", "child1_date_vacc_card_BCG",
                    "child1_date_vacc_card_Penta1", "child1_date_vacc_card_Penta2", "child1_date_vacc_card_Penta3", "child1_date_vacc_card_MR1",
                    "child1_date_vacc_card_MR2")

relColNamesVec2 <- c("KEY", "SubmissionDate", "eligibility_check_pregnant", "eligibility_check_child", "child1s_name", "child1s_dob",
                    "mothers_name", "mothers_contact_number", "fathers_name", "fathers_contact_number",
                    "alternate_contact_1_name", "alternate_contact1_number", "alternate_contact_2_name",
                    "alternate_contact2_contact_number")

# subsetting the dataset to get the above columns
follUpSurvRawDataRelCol <- select(follUpSurvRawData, relColNamesVec2)

# randomly selecting 20% of the data from the entire dataset---------
rndNum <- sample(1:nrow(follUpSurvRawDataRelCol), 0.2*(nrow(follUpSurvRawDataRelCol)))
follUpSurvRawDataRelCol_rand <- follUpSurvRawDataRelCol[rndNum, ]
follUpSurvRawDataRelCol_rand$scto_key <- 1:nrow(follUpSurvRawDataRelCol_rand)
follUpSurvRawDataRelCol_rand <- select(follUpSurvRawDataRelCol_rand, scto_key, everything())
colnames(follUpSurvRawDataRelCol_rand)[2] <- "uuid"

# write the csv file which will serve as the input to the follow up survey data
write.csv(follUpSurvRawDataRelCol_rand, "followUpSurveyPrefillDataExp_June102019.csv")
