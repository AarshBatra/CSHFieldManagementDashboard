## vaccination card photo review----------------------

# Variables to be updated in the file whenever we run this--------
# 1. dateInput, dateRange (and the corresponding tmp_getDateRange)
# 2. teamName
# 3. Input to the vacc card 'csv' file will be different based on the whose organization's M&E report is being generated
# 4. Enrollment Survey Dataset
# 5. Check for updates in the 'member_name' until sign up form is cleaned up. Given there are updates, change the teamMembers variable accordingly, and then run the code.
# 6. Update the "helper.R" file's, 'getPartnerTeam' function after going through the member names and seeing if some new names are added.
# 7. Update the path where the final output ('ds_nonNA_mcpCard', 'ds_nonNA_vaccCard', 'ds_nonNA_vaccReceipt') of this code will be stored, this depends on the 'teamName'. The files should be outputted to the folder that corresponds to its team name.
# 8. set what % of vaccine card back checking needs to be done in the variable called: 'vaccBackCheckPerc'

## relevant files and libraries------------------
source("helper.R")
ds <- read.csv("Enrollment survey D2D partners 20190223_WIDE.csv")
ds <- as_tibble(ds)
ds$SubmissionDate <- lubridate::mdy_hms(ds$SubmissionDate)

## deciding the organization for which we have to get the audio audit data------
teamName <- "d2dinhouse"
teamMembers <- getPartnerTeam(teamName) # this function is present in the helper file

## percent of vaccine card back checks to be done----------
vaccBackCheckPerc <- 0.7

## for what date Range do we want to generate the data for-----------
dateInput <- "2019-08-09"
tmp_getDateRange <- lubridate::date("2019-07-25") : lubridate::date("2019-07-30")
tmp_getDateRange <- lubridate::as_date(tmp_getDateRange)
dateRange <- tmp_getDateRange # we can use just this for both single day reports and date range
dateRange <- as.character(dateRange)  

## filter data and select relevant colums to be exported to the Google Sheet-----------------

# columns with the word "consent" in it
ds_colnames <- colnames(ds)
ds_colnames_consent <- str_get(ds_colnames, "consent")

# columns with the word "photo" in it
ds_colnames <- colnames(ds)
ds_colnames_photo <- str_get(ds_colnames, "photo")

# select relevant columns
relCol <- c("KEY" ,"member_name", "member_organization", "mothers_name", 
            "eligibility_check_pregnant", "eligibility_check_child", "child1s_name",
            "child1s_dob")

photo_col_mcp_card <- c("mcp_card_photo_due_date", "mcp_card_photo_last_menstrual_period",
                        "mcp_card_photo_last_prenatal_visit")

photo_col_vacc_card <- c("child1s_dob_vacc_card_photo", "child1_vacc_card_photo")

photo_col_receipt <- c("child1_date_last_vacc_photo")

# getting the data for a single day: dateInput------ (use the date range option right below this block of code)
# ds$dateOnly <- str_extract(ds$SubmissionDate, "..........")
# ds <- filter(ds, dateOnly == dateInput)

# getting the data for a date range-----------
ds_dateRange <- mutate(ds, relDateRangeOnly = str_extract(
  SubmissionDate, "(....-..-..)|(....-..-.)"))
ds_dateRange <- dplyr::filter(ds_dateRange, !is.na(relDateRangeOnly))
ds_dateRange <- dplyr::filter(ds_dateRange, relDateRangeOnly %in% dateRange)
ds <- ds_dateRange

# selecting relevant columns from the datasets---------
ds_nonNA_mcpCard <- dplyr::select(ds, mcp_card_photo_consent, relCol, photo_col_mcp_card)
ds_nonNA_vaccCard <- dplyr::select(ds, child1s_vacc_card_photo_consent, relCol, photo_col_vacc_card)
ds_nonNA_vaccReceipt <- dplyr::select(ds, child1_vacc_receipt_photo_consent, relCol, photo_col_receipt)

# Making 3 different datasets for MCP Card, Vaccination Card, Vaccination Receipts
ds_nonNA_mcpCard <- filter(ds_nonNA_mcpCard, !is.na(mcp_card_photo_consent), mcp_card_photo_consent == 1)
ds_nonNA_vaccCard <- filter(ds_nonNA_vaccCard, !is.na(child1s_vacc_card_photo_consent), child1s_vacc_card_photo_consent == 1)
ds_nonNA_vaccReceipt <- filter(ds_nonNA_vaccReceipt, !is.na(child1_vacc_receipt_photo_consent), child1_vacc_receipt_photo_consent == 1)

##------------------- Randomizing code for each of the three datasets -------------------##

# ('1': ds_nonNA_mcpCard) randomly selecting x% (x = 30, as of now) of the rows for each surveyor-------
group_by_surveyor_1 <- group_by(ds_nonNA_mcpCard, member_name)
surv_summary_1 <- dplyr::summarise(group_by_surveyor_1, num_surveys_done = length(member_name))

indexes_surveyor_1 <- surv_summary_1$num_surveys_done
indexes_surveyor_name_1 <- surv_summary_1$member_name
listOfIndexesForEachSurveyor <- list()

dummy_var_1 <- c()
dummy_var_2 <- c()
for(i in 1:length(indexes_surveyor_name_1)){
  if(i == 1){
    dummy_var_1 <- indexes_surveyor_1[1]
    listOfIndexesForEachSurveyor[[i]] <- c(1 : dummy_var_1) 
  } else {
    dummy_var_2 <- dummy_var_1 + 1
    dummy_var_1 <- dummy_var_1 + indexes_surveyor_1[i] 
    listOfIndexesForEachSurveyor[[i]] <- c(dummy_var_2 : dummy_var_1)
  }
}

# random indices
randomIndicesList <- list()
for(j in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesList[[j]] <- sample(listOfIndexesForEachSurveyor[[j]], 
                                   ((length(listOfIndexesForEachSurveyor[[j]]))*(vaccBackCheckPerc)))
}

# random indices accumulate
randomIndicesFinal_singleDate <- c()
for(k in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesFinal_singleDate <-  append(randomIndicesFinal_singleDate, randomIndicesList[[k]])
}

ds_nonNA_mcpCard <- ds_nonNA_mcpCard[randomIndicesFinal_singleDate, ]


# ('2': ds_nonNA_vaccCard) randomly selecting x% of the rows for each surveyor-------
group_by_surveyor_2 <- group_by(ds_nonNA_vaccCard, member_name)
surv_summary_2 <- dplyr::summarise(group_by_surveyor_2, num_surveys_done = length(member_name))

indexes_surveyor_2 <- surv_summary_2$num_surveys_done
indexes_surveyor_name_2 <- surv_summary_2$member_name
listOfIndexesForEachSurveyor <- list()

dummy_var_1 <- c()
dummy_var_2 <- c()
for(i in 1:length(indexes_surveyor_name_2)){
  if(i == 1){
    dummy_var_1 <- indexes_surveyor_2[1]
    listOfIndexesForEachSurveyor[[i]] <- c(1 : dummy_var_1) 
  } else {
    dummy_var_2 <- dummy_var_1 + 1
    dummy_var_1 <- dummy_var_1 + indexes_surveyor_2[i] 
    listOfIndexesForEachSurveyor[[i]] <- c(dummy_var_2 : dummy_var_1)
  }
}

# random indices
randomIndicesList <- list()
for(j in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesList[[j]] <- sample(listOfIndexesForEachSurveyor[[j]], 
                                   ((length(listOfIndexesForEachSurveyor[[j]]))*(vaccBackCheckPerc)))
}

# random indices accumulate
randomIndicesFinal_singleDate <- c()
for(k in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesFinal_singleDate <-  append(randomIndicesFinal_singleDate, randomIndicesList[[k]])
}

ds_nonNA_vaccCard <- ds_nonNA_vaccCard[randomIndicesFinal_singleDate, ]


# ('3': ds_nonNA_vaccReceipt) randomly selecting x% of the rows for each surveyor-------
group_by_surveyor_3 <- group_by(ds_nonNA_vaccReceipt, member_name)
surv_summary_3 <- dplyr::summarise(group_by_surveyor_3, num_surveys_done = length(member_name))

indexes_surveyor_3 <- surv_summary_3$num_surveys_done
indexes_surveyor_name_3 <- surv_summary_3$member_name
listOfIndexesForEachSurveyor <- list()

dummy_var_1 <- c()
dummy_var_2 <- c()
for(i in 1:length(indexes_surveyor_name_3)){
  if(i == 1){
    dummy_var_1 <- indexes_surveyor_3[1]
    listOfIndexesForEachSurveyor[[i]] <- c(1 : dummy_var_1) 
  } else {
    dummy_var_2 <- dummy_var_1 + 1
    dummy_var_1 <- dummy_var_1 + indexes_surveyor_2[i] 
    listOfIndexesForEachSurveyor[[i]] <- c(dummy_var_2 : dummy_var_1)
  }
}

# random indices
randomIndicesList <- list()
for(j in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesList[[j]] <- sample(listOfIndexesForEachSurveyor[[j]], 
                                   ((length(listOfIndexesForEachSurveyor[[j]]))*(vaccBackCheckPerc)))
}

# random indices accumulate
randomIndicesFinal_singleDate <- c()
for(k in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesFinal_singleDate <-  append(randomIndicesFinal_singleDate, randomIndicesList[[k]])
}

ds_nonNA_vaccReceipt <- ds_nonNA_vaccReceipt[randomIndicesFinal_singleDate, ]


##------------------- Randomizing code for each of the three datasets -------------------##

# filtering out data only for idi-------------
# idiTeam <- c("Mahendra", "Pradeep kumar mishra", "Rajan singh", "Suchita tiwari", "Sangeeta Devi", "Brahm Dev Verma")

# Vaccination Card: moving around columns for populating the right columns of the Google Sheet---------
ds_nonNA_vaccCard <- select(ds_nonNA_vaccCard, KEY, member_name, member_organization, 
                            everything())

ds_nonNA_vaccCard <- filter(ds_nonNA_vaccCard, member_name %in% teamMembers)

# MCP Card: moving around columns for populating the right columns of the Google Sheet
ds_nonNA_mcpCard <- ds_nonNA_mcpCard[, 2:ncol(ds_nonNA_mcpCard)]
ds_nonNA_mcpCard <- filter(ds_nonNA_mcpCard, member_name %in% teamMembers)

# Vaccination Receipt: moving around columns for populating the right columns of the Google Sheet
ds_nonNA_vaccReceipt <- ds_nonNA_vaccReceipt[, 2:ncol(ds_nonNA_vaccReceipt)]
ds_nonNA_vaccReceipt <- filter(ds_nonNA_vaccReceipt, member_name %in% teamMembers)



write.csv(ds_nonNA_vaccCard, "vaccCardCheckOutputFiles/d2dinhouse/ds_nonNA_vaccCard_forDoingBackChecks_July25-302019.csv")
write.csv(ds_nonNA_mcpCard, "vaccCardCheckOutputFiles/d2dinhouse/ds_nonNA_mcpCard_forDoingBackChecks.csv_July25-302019.csv")
write.csv(ds_nonNA_vaccReceipt, "vaccCardCheckOutputFiles/d2dinhouse/ds_nonNA_vaccReceipt_forDoingBackChecks_July25-302019.csv")



## fill in Google Sheets------------------

title <- "v1 Vaccination Card Photo Review D2D Partners " 
tab_1 <- "Vaccination Card"
tab_2 <- "MCP card"
tab_3 <- "Vaccination Receipt"
mySheets <- googlesheets::gs_ls()
filter(mySheets, sheet_title == title)

# register sheet
vaccCardReviewGSheet <- gs_title(title) # first argument to every function in this package

# add vaccination card data to the "Vaccination Card" tab--------
for(j in 1:nrow(ds_nonNA_vaccCard)){
  gs_add_row(vaccCardReviewGSheet, ws = tab_1, ds_nonNA_vaccCard[j, ])
}

# add MCP card data to the "MCP card" tab--------
for(k in 1:nrow(ds_nonNA_mcpCard)){
  gs_add_row(vaccCardReviewGSheet, ws = "MCP card", ds_nonNA_mcpCard[k, ])
}

# add vaccination receipt data to the "Vaccination Receipt" tab--------
for(l in 1:nrow(ds_nonNA_vaccReceipt)){
  gs_add_row(vaccCardReviewGSheet, ws = tab_3, ds_nonNA_vaccReceipt[l, ])
}
