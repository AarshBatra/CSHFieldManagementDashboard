## Nexmo testing results-------------

# library---------
library(tidyverse)
library(dplyr)
library(readr)
library(lubridate)

#==================== using the enrollment survey dataset ========================


# get processed data from the checks file----------
source("checks.R")

# get processed dataset from the checks file output for Nexmo manipulation-------
ds_wide_expand <- mutate(ds_wide_expand, dateOnlyDateType = as_date(dateOnly))
prefillNexmoDataMaster <- filter(ds_wide_expand, (today() - dateOnlyDateType) >= 0, (today() - dateOnlyDateType) <= 14)

# further filtering the dataset and randomly selecting 300 people--------
numRandEntChoose <- 300
prefillNexmoDataSubset <- sample_n(prefillNexmoDataMaster, numRandEntChoose)

# relevant columns to be exported--------
relCol <- c("KEY", "SubmissionDate", "member_name", "member_organization", "mothers_name", "mothers_contact_number", "fathers_name", "fathers_contact_number")

# select these columns from the randomized pre fill dataset---------
prefillNexmoDataSubset_rand <- select(prefillNexmoDataSubset, relCol)

# export Nexmo prefill randomized data for Nexmo testing-------
write.csv(prefillNexmoDataSubset_rand, "prefillNexmoRandJuly222019.csv")

#================================================================================




# get data after the Nexmo test is done for analysis========================

batch1Data <- read_csv("Nexmo Testing Round 2  - Batch1.csv")
batch2Data <- read_csv("Nexmo Testing Round 2  - Batch2.csv")

# combine two rounds data in a single dataset
batch1batch2Data <- rbind(batch1Data, batch2Data)

# add date only column
batch1batch2Data <- mutate(batch1batch2Data, dateOnly = str_extract(SubmissionDate, ".........."))
batch1batch2Data$dateOnly <- lubridate::dmy(batch1batch2Data$dateOnly)
batch1batch2Data <- dplyr::arrange(batch1batch2Data, dateOnly)
batch1batch2Data <- dplyr::filter(batch1batch2Data, !is.na(KEY), !is.na(dateOnly))

## using only 2 batches================================================================
batch1batch2Data$batch <- c(rep(NA, times = nrow(batch1batch2Data)))
d2dStartDate <- as_date("2019-02-25")
for(i in 1:nrow(batch1batch2Data)){
  if((i >= 1) && (i <= 276)){
    batch1batch2Data$batch[i] <- 1
  } else if((i > 276) && (i <= nrow(batch1batch2Data)) ){
    batch1batch2Data$batch[i] <- 2
  }
}

batch1batch2Data_grp_batch <- group_by(batch1batch2Data, batch)
batch1batch2Data_summ <- dplyr::summarise(batch1batch2Data_grp_batch, daysPassedSinceStart = (dateOnly[n()] - d2dStartDate), count_unreachable = count_if("undeliverable:(Subscriber UnReachable)", Reachable),
                                          count_reachable = count_if("reachable:(Subscriber Reachable)", Reachable), count_statusUnknown = count_if("unknown:(Status Unknown)", Reachable), batchTotal = n(), dist = mean((today() - dateOnly), na.rm = TRUE))
batch1batch2Data_summ <- mutate(batch1batch2Data_summ, percUnreachable = (count_unreachable/batchTotal)*100)



staticDateRangeVec <- c("0 to 2 months", "2 months to 5 months")
batch1batch2Data_summ <- mutate(batch1batch2Data_summ, `timePeriod(0 months = start date)` = staticDateRangeVec)

ggplot(batch1batch2Data_summ) + geom_bar(mapping = aes(x = `timePeriod(0 months = start date)`, y = percUnreachable), stat = "identity") + geom_text(aes(label=..count..), stat="count", vjust= 0.5, hjust = 2, size= 5)





## using the 5 batch method===================================================================

# assigning batch numbers (combining 100 numbers in a batch chronologically)-----
batch1batch2Data$batch <- c(rep(NA, times = nrow(batch1batch2Data)))
d2dStartDate <- as_date("2019-02-25")
batchSize <- 100
for(i in 1:nrow(batch1batch2Data)){
  if(i >= 1 && i <= 100){
    batch1batch2Data$batch[i] <- 1
  } else if (i >= 101 && i <= 200) {
    batch1batch2Data$batch[i] <- 2
  } else if (i >= 201 && i <= 300){
    batch1batch2Data$batch[i] <- 3
  } else if (i >= 301 && i <= 400){
    batch1batch2Data$batch[i] <- 4
  } else if (i >= 401 && i <= nrow(batch1batch2Data)){
    batch1batch2Data$batch[i] <- 5
  }
}

batch1batch2Data_grp_batch <- group_by(batch1batch2Data, batch)
batch1batch2Data_summ <- dplyr::summarise(batch1batch2Data_grp_batch, daysPassedSinceStart = (dateOnly[n()] - d2dStartDate), count_unreachable = count_if("undeliverable:(Subscriber UnReachable)", Reachable),
count_reachable = count_if("reachable:(Subscriber Reachable)", Reachable), count_statusUnknown = count_if("unknown:(Status Unknown)", Reachable), batchTotal = n())
batch1batch2Data_summ <- mutate(batch1batch2Data_summ, percUnreachable = count_unreachable/batchTotal)


plt <- ggplot(batch1batch2Data_summ) + geom_line(mapping = aes(x = daysPassedSinceStart, y = percUnreachable))
ggplotly(plt)














#=================================================================================


#================using the telerivet database (not using as of now, some confusion)===============


# get contacts database--------
contactsDatabase <- read_csv("Contacts.csv")

# getting some columns in the right format--------
contactsDatabase$`Date of Sign Up` <- lubridate::as_date(contactsDatabase$`Date of Sign Up`)

# getting the subset of the contacts data: those people who signed up 3-9 months ago via door to door---------
contactsDatabaseSubset <- filter(contactsDatabase, (today() - `Date of Sign Up`) >= 90, (today() - `Date of Sign Up`) <= 270, 
                                 `Method of Sign Up` == "Door To Door")

# further filtering the dataset and randomly selecting 300 people--------
numRandEntChoose <- 300
contactsDatabaseSubset_random <- sample_n(contactsDatabaseSubset, numRandEntChoose)

# relevant columns to be exported--------
relCol <- c("Contact ID", "Name", "Date of Sign Up", "Phone Number", "Last Contacted", "Functional DoB", "Date of Birth")

# selecting relevant columns which will then be exported to the Nexmo Google Spreadsheet------
contactsDatabaseSubset_random <- select(contactsDatabaseSubset_random, relCol)


