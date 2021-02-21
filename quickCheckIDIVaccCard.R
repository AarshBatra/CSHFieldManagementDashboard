# Quick Check Vaccination Rates IDI Pilot

# libraries--------------
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)
library(lubridate)
library(expss)
library(ggplot2)
library(plotly)
library(googlesheets)

# file Name----------
vaccCardBackCheckFileName <-  "v1 Vaccination Card Photo Review D2D Partners IDI Pilot - Vaccination Card.csv"

# read in the vaccination card photo Review dataset-----------
vaccCardPhotoReviewData <- read_csv(vaccCardBackCheckFileName)

# cleaning the vaccination card photo review dataset-----------
colnames(vaccCardPhotoReviewData)[19:ncol(vaccCardPhotoReviewData)] <- as.character(vaccCardPhotoReviewData[2, (19:ncol(vaccCardPhotoReviewData))]) 
colnames(vaccCardPhotoReviewData)[ncol(vaccCardPhotoReviewData)] <- "Comments"
vaccCardPhotoReviewData <- vaccCardPhotoReviewData[(3:nrow(vaccCardPhotoReviewData)), ]

# getting subset of population for which we have DOB---------
vaccCardPhotoReviewData_nonNADOB <- dplyr::filter(vaccCardPhotoReviewData, !is.na(`Child DOB
Enter if:  "Is there a child DOB" == 1`))


# changing column types to type date-----------
vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1`[1:22] <- dmy(vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1`[1:22])

vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1`[23:118] <- ymd(vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1`[23:118])

foo <- as.integer(vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1`)

foo <- as_date(foo)

vaccCardPhotoReviewData_nonNADOB$`Child DOB
Enter if:  "Is there a child DOB" == 1` <- foo

# focussing on kids older than 6 months---------
colnames(vaccCardPhotoReviewData_nonNADOB)[14] <- "chDOBVaccCard"
colnames(vaccCardPhotoReviewData_nonNADOB)[33] <- "p1Date"
colnames(vaccCardPhotoReviewData_nonNADOB)[39] <- "p2Date"
colnames(vaccCardPhotoReviewData_nonNADOB)[45] <- "p3Date"
colnames(vaccCardPhotoReviewData_nonNADOB)[31] <- "p1Marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[37] <- "p2Marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[43] <- "p3Marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[22] <- "opv0marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[24] <- "opv0date"
colnames(vaccCardPhotoReviewData_nonNADOB)[28] <- "opv1marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[30] <- "opv1date"
colnames(vaccCardPhotoReviewData_nonNADOB)[34] <- "opv2marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[36] <- "opv2date"
colnames(vaccCardPhotoReviewData_nonNADOB)[40] <- "opv3marked"
colnames(vaccCardPhotoReviewData_nonNADOB)[42] <- "opv3date"







vaccCardPhotoReviewData_nonNADOB <- dplyr::mutate(vaccCardPhotoReviewData_nonNADOB, ageInDays = today() - chDOBVaccCard)
vaccCardPhotoReviewData_nonNADOB_oldThan6Months <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB, ageInDays > 180)

# PENTA 1, 2, 3 rates for children 6 month and above----------
vaccCardPhotoReviewData_nonNADOB_oldThan6Months <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months)



vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP1 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months, (p1Marked == "Date" | p1Marked == "Marking (Not a Date)"))
vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP2 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months, (p2Marked == "Date" | p2Marked == "Marking (Not a Date)"))
vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP3 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months, (p3Marked == "Date" | p3Marked == "Marking (Not a Date)"))


percGotP1 <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP1)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months)
percGotP2 <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP2)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months)
percGotP3 <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_gotP3)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months)




# Separate Analysis: If there is no Penta 1 date, but there is an OPV1 date assume that there is a Penta 1 date

vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP <- vaccCardPhotoReviewData_nonNADOB_oldThan6Months
for(i in 1:nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP)){
 if((vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv1marked[i] == "Date" || vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv1marked[i] == "Marking (Not a Date)") && (is.na(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p1Date[i] == TRUE))){
   vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p1Marked[i] <- "Date" 
 }
  
if((vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv2marked[i] == "Date" || vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv2marked[i] == "Marking (Not a Date)") && (is.na(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p2Date[i] == TRUE))){
  vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p2Marked[i] <- "Date" 
}

if((vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv3marked[i] == "Date" || vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$opv3marked[i] == "Marking (Not a Date)") && (is.na(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p3Date[i] == TRUE))){
  vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP$p3Marked[i] <- "Date" 
}


}

vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP1 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP, (p1Marked == "Date" | p1Marked == "Marking (Not a Date)"))
vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP2 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP, (p2Marked == "Date" | p2Marked == "Marking (Not a Date)"))
vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP3 <- dplyr::filter(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP, (p3Marked == "Date" | p3Marked == "Marking (Not a Date)"))


percGotP1_SEP <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP1)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP)
percGotP2_SEP <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP2)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP)
percGotP3_SEP <- nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP_gotP3)/nrow(vaccCardPhotoReviewData_nonNADOB_oldThan6Months_SEP)



