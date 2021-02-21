# : getting relevant files and exporting to Google Sheets--------------------------------

# libraries
library(googlesheets)
library(tidyverse)
library(stringr)
library(lubridate)

# helper file
# source("dailyMonitoringCode.R")

# get up to date data--
wideData <- read.csv("Enrollment survey D2D partners 20190223_WIDE.csv")
wideData <- as_tibble(wideData)


# adding a duration of survey data column
wideData$SubmissionDate <- lubridate::mdy_hms(wideData$SubmissionDate)
wideData$starttime <- lubridate::mdy_hms(wideData$starttime)
wideData$endtime <- lubridate::mdy_hms(wideData$endtime)

wideData <- wideData %>% mutate(durationOfSurveyInMinutes = interval(endtime, starttime))
wideData$durationOfSurveyInMinutes <- as.duration(wideData$durationOfSurveyInMinutes)
wideData$durationOfSurveyInMinutes <- (abs(as.numeric(wideData$durationOfSurveyInMinutes)))/60

# adding a date only column
# wideData <- wideData %>% mutate(dateOnly = str_extract(SubmissionDate, "....-..-.."))

# choosing only those in which total # of cg = 1--
# wideData <- filter(wideData, caregiver_count == 1)

wideDataSS <- select(wideData, SubmissionDate, starttime, endtime, member_name, complete_audio_audit, durationOfSurveyInMinutes)

# Option 1: getting today's audio files--
date <- "2019-03-12"
indexes_date <- str_detect(wideDataSS$SubmissionDate, date)
wideDataSS_sD <- wideDataSS[indexes_date, ]

# Option 2: getting audio files for a Date Range (as of now we want daily output, so date range is composed of only one date)
dateRange <- c("2019-06-11")

dateRangeStatic <- c("June 11, 2019")

wideDataSS_dR <- mutate(wideDataSS, relDateRangeOnly = str_extract(
  SubmissionDate, "(....-06-..)|(....-06-.)"))

wideDataSS_dR <- filter(wideDataSS_dR, !(is.na(relDateRangeOnly)))
wideDataSS_dR <- filter(wideDataSS_dR, relDateRangeOnly %in% dateRange)

# classifying by surveyor--
wideDataSS_dR <- arrange(wideDataSS_dR, member_name)
wideDataSS_dR_lessthan5 <- filter(wideDataSS_dR, durationOfSurveyInMinutes < 5)

# summarizing information
wideDataSS_dR_sum <- dplyr::group_by(wideDataSS_dR, member_name)
wideDataSS_dR_sum <- dplyr::summarize(wideDataSS_dR_sum, countTot = n())

wideDataSS_dR_lessthan5_sum <- dplyr::group_by(wideDataSS_dR_lessthan5, member_name)
wideDataSS_dR_lessthan5_sum <- dplyr::summarize(wideDataSS_dR_lessthan5_sum, countLessThan5 = n())

# joining 2 summary tables from above
summaryTableDuration <- left_join(wideDataSS_dR_sum, wideDataSS_dR_lessthan5_sum, by = "member_name")
summaryTableDuration <- mutate(summaryTableDuration, percentageLessThan5minutes = (countLessThan5/countTot)*100)

# adding a static timeline column
summaryTableDuration <- mutate(summaryTableDuration, timeline = dateRangeStatic)
summaryTableDuration <- select(summaryTableDuration, timeline, everything())
write.csv(summaryTableDuration, "audioAuditOutputFiles/audioAuditDurationOfSurveySummary_20190611.csv")

write.csv(wideDataSS_dR_lessthan5, "Mr1.csv")
# exporting final columns to either a excel file/directly to Google Sheets--

# list sheets
title <- "v1 Audio Audits D2D Partners Survey" 
tab <- "audio audit duration summary"
mySheets <- googlesheets::gs_ls()
filter(mySheets, sheet_title == title)

# register sheet
audioAuditGSheet <- gs_title(title) # first argument to every function in this package

# open registered Google Sheet in browser--
# gs_browse(audioAuditGSheet, ws = tab)

# Inspect a sheet
gs_ws_ls(audioAuditGSheet)

# Read all data of a worksheet in a data frame
# gs_df <- gs_read(audioAuditGSheet, ws = tab)

# target specific range of cells
# gs_df_spec_range <- gs_read(audioAuditGSheet, ws = tab, range = "A13:E593")

# add a new worksheet to an existing Google Sheet
# gs_ws_new(audioAuditGSheet, ws_title = "test_out_1", input = summaryTableDuration)


# add data to the audio audit sheet--

for(i in 1:nrow(summaryTableDuration)){
  gs_add_row(audioAuditGSheet, ws = tab, summaryTableDuration[i, ])
}   






