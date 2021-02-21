# audio audits: getting relevant files and exporting to Google Sheets--------------------------------

# variables to be updated in the file whenever we run this--------
# 1. dateRange (and the corresponding tmp_getDateRange)
# 2. teamName
# 3. Input to the audio audit 'csv' file will be different based on the whose organization's M&E report is being generated
# 4. Enrollment Survey Dataset
# 5. Check for updates in the 'member_name' until sign up form is cleaned up. Given there are updates, change the teamMembers variable accordingly, and then run the code.
# 6. Update the "helper.R" file's, 'getPartnerTeam' function after going through the member names and seeing if some new names are added.
# 7. Update the path where the final output ('finalDataCombRand') of this code will be stored, this depends on the 'teamName'. The files should be outputted to the folder that corresponds to its team name.
# 8. Set the % of long surveys that needs to be audited ('ls_perc') and also the % of short surveys that needs to be audited ('ss_perc').

# libraries and helper files------------
library(googlesheets)
library(tidyverse)
library(stringr)
library(lubridate)
source("helper.R")

# helper file-------------
# source("dailyMonitoringCode.R")

# get up to date data--------------
wideData <- read.csv("Enrollment survey D2D partners 20190223_WIDE.csv")
wideData <- as_tibble(wideData)

# getting the data for a specific date, a date range----------
date <- "2019-07-03"
tmp_getDateRange <- lubridate::date("2019-07-30") : lubridate::date("2019-08-06")
tmp_getDateRange <- lubridate::as_date(tmp_getDateRange)
dateRange <- tmp_getDateRange # just use this for both single date and for date ranges, do not use the above 'date' variable.
dateRange <- as.character(dateRange)

# setting up a specific team--------
teamName <- "d2dinhouse"
teamMembers <- getPartnerTeam(teamName) # this function is present in the helper file

# setting up as to what % of surveys need to be audited. Need to specify two numbers
# one for long surveys, other for short surveys.
ls_perc <- 0.30
ss_perc <- 0.30
  
# adding a duration of survey data column----------------
wideData$SubmissionDate <- lubridate::mdy_hms(wideData$SubmissionDate)
wideData$starttime <- lubridate::mdy_hms(wideData$starttime)
wideData$endtime <- lubridate::mdy_hms(wideData$endtime)

wideData <- wideData %>% mutate(durationOfSurveyInMinutes = interval(endtime, starttime))
wideData$durationOfSurveyInMinutes <- as.duration(wideData$durationOfSurveyInMinutes)
wideData$durationOfSurveyInMinutes <- (abs(as.numeric(wideData$durationOfSurveyInMinutes)))/60

# adding a date only column
# wideData <- wideData %>% mutate(dateOnly = str_extract(SubmissionDate, "....-..-.."))

# choosing only those in which total # of cg = 1--
# wideDataColSubExpForAA <- filter(wideData, caregiver_count == 1)

wideDataColSubExpForAA <- select(wideData, KEY, SubmissionDate, starttime, endtime, member_name, complete_audio_audit, durationOfSurveyInMinutes)

# Option 1: getting today's audio files--
indexes_date <- str_detect(wideDataColSubExpForAA$SubmissionDate, date)
wideDataColSubExpForAA_singleDate <- wideDataColSubExpForAA[indexes_date, ]

# Option 2: getting audio files for a Date Range (as of now we want daily output, so date range is composed of only one date)
wideDataColSubExpForAA_dateRange <- mutate(wideDataColSubExpForAA, relDateRangeOnly = str_extract(
  SubmissionDate, "(....-..-..)|(....-..-.)"))

wideDataColSubExpForAA_dateRange <- filter(wideDataColSubExpForAA_dateRange, !(is.na(relDateRangeOnly)))
wideDataColSubExpForAA_dateRange <- filter(wideDataColSubExpForAA_dateRange, relDateRangeOnly %in% dateRange)

# classifying by surveyor-------
threshold_dur <- 50 # anything above this means that the surveyors are submitting surveys at wrong times, we don't care about that here.
wideDataColSubExpForAA_dateRange <- arrange(wideDataColSubExpForAA_dateRange, member_name)
wideDataColSubExpForAA_dateRange_lessthanXMinutes <- filter(wideDataColSubExpForAA_dateRange, durationOfSurveyInMinutes < threshold_dur)

# fill in the member name for the ones that are blank----------
ind_mem_name_blank <- str_detect(wideDataColSubExpForAA_dateRange_lessthanXMinutes$member_name, "")
wideDataColSubExpForAA_dateRange_lessthanXMinutes$member_name <- as.character(wideDataColSubExpForAA_dateRange_lessthanXMinutes$member_name)
wideDataColSubExpForAA_dateRange_lessthanXMinutes[!ind_mem_name_blank, ]$member_name <- c(rep("blank", times = 7))
 
# group by member_name
group_by_surveyor <- group_by(wideDataColSubExpForAA_dateRange_lessthanXMinutes, member_name) # jogging my memory
surv_summary <- dplyr::summarize(group_by_surveyor, num_surveys_done = length(member_name)) # jogging my memory

indexes_surveyor <- surv_summary$num_surveys_done
indexes_surveyor_name <- surv_summary$member_name
listOfIndexesForEachSurveyor <- list()

dummy_var_1 <- c()
dummy_var_2 <- c()
for(i in 1:length(indexes_surveyor_name)){
  if(i == 1){
    dummy_var_1 <- indexes_surveyor[1]
    listOfIndexesForEachSurveyor[[i]] <- c(1 : dummy_var_1) 
  } else {
    dummy_var_2 <- dummy_var_1 + 1
    dummy_var_1 <- dummy_var_1 + indexes_surveyor[i] 
    listOfIndexesForEachSurveyor[[i]] <- c(dummy_var_2 : dummy_var_1)
  }
}

# random indices
randomIndicesList <- list()
for(j in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesList[[j]] <- sample(listOfIndexesForEachSurveyor[[j]], 
                                   ((length(listOfIndexesForEachSurveyor[[j]]))*(ls_perc)))
}

# random indices accumulate
randomIndicesFinal_singleDate <- c()
for(k in 1:length(listOfIndexesForEachSurveyor)){
  randomIndicesFinal_singleDate <-  append(randomIndicesFinal_singleDate, randomIndicesList[[k]])
}

wideDataColSubExpForAA_dateRange_random <- wideDataColSubExpForAA_dateRange[randomIndicesFinal_singleDate, ]
wideDataColSubExpForAA_dateRange_random <- arrange(wideDataColSubExpForAA_dateRange_random,
                                                   SubmissionDate)

wideDataColSubExpForAA_dateRange_random <- mutate(wideDataColSubExpForAA_dateRange_random,
                                                  dateInDateFormat = lubridate::ymd(relDateRangeOnly) )

wideDataColSubExpForAA_dateRange_random <- arrange(wideDataColSubExpForAA_dateRange_random, dateInDateFormat)
View(wideDataColSubExpForAA_dateRange_random)

quickCheckExp <- filter(wideDataColSubExpForAA_dateRange_random, durationOfSurveyInMinutes <= threshold_dur)
quickCheckExp <- select(quickCheckExp, SubmissionDate, starttime, endtime, member_name, complete_audio_audit, 
                        durationOfSurveyInMinutes)

wideDataColSubExpForAA_dateRange_random <- select(wideDataColSubExpForAA_dateRange_random, KEY, SubmissionDate, starttime, endtime, 
                                                  member_name, complete_audio_audit, durationOfSurveyInMinutes)


# Short surveys: for each surveyor count how many surveys are less than 'y' minutes in length,--------
# here 'y' is a threshold filter. If the number of surveys that are less than equal
# to 4 minutes is greater than equal to 4 (per surveyor) add 25% of the short surveys
# to 'wideDataColSubExpForAA_dateRange_random', after that remove the duplicates
# from the list of surveys.---------

# figuring out the number of surveys less than 'y' minutes per surveyor
wideDataColSubExpForAA_dateRange_shortSurGrp <- group_by(wideDataColSubExpForAA_dateRange, 
                                                         member_name)
threshold_y <- 20 # in minutes
threshold_shSurCountThresh <- 20

wideDataColSubExpForAA_dateRange_shortSurSumm <- dplyr::summarise(wideDataColSubExpForAA_dateRange_shortSurGrp, 
                                                           countLessThanYminutes = sum(durationOfSurveyInMinutes <= threshold_y, na.rm = TRUE),
                                                           numSurveysDone = n())
wideDataColSubExpForAA_dateRange_shortSurSumm_ss <- filter(wideDataColSubExpForAA_dateRange_shortSurSumm, countLessThanYminutes > threshold_shSurCountThresh)

ss_filterOutSS <- filter(wideDataColSubExpForAA_dateRange, member_name %in% wideDataColSubExpForAA_dateRange_shortSurSumm_ss$member_name, durationOfSurveyInMinutes <= threshold_y) 
ss_indices <- sample(1:nrow(ss_filterOutSS), ((nrow(ss_filterOutSS))*(ss_perc))) 
ss_filterOutSS <- ss_filterOutSS[ss_indices, ]
ss_filterOutSS <- select(ss_filterOutSS, colnames(wideDataColSubExpForAA_dateRange_random))

# combining the very short randomly selected surveys with other randomly selected surveys
# and then keeping the unique ones----
finalDataCombRand <- rbind(wideDataColSubExpForAA_dateRange_random, ss_filterOutSS)
finalDataCombRand <- unique(finalDataCombRand)

# idiTeam <- c("Mahendra", "Pradeep kumar mishra", "Rajan singh", "Suchita tiwari", "Sangeeta Devi", "Brahm Dev Verma")
finalDataCombRand <- filter(finalDataCombRand, member_name %in% teamMembers)


write.csv(finalDataCombRand, "audioAuditOutputFiles/d2dinhouse/audioAuditDataForBackChecks_July30ToAug092019.csv")
# wideDataColSubExpForAA_dateRange_random

## this code under this comment was written specifically for a single
# task, so this is not to be used in general=======================

# generating a quick audio audit check dataset for checking if we can hear the 
# instruction "you have to take one pill a day" in the audio-------------

wideData$Sb
wideData <- mutate(wideData, dateOnly = mdy(str_extract(SubmissionDate, "...........|............")))
foo <- filter(wideData, SubmissionDate > "2019-07-30"  , SubmissionDate < "2019-08-07")
write.csv(foo, "audioAuditQuickChecPNS.csv")




# exporting final columns to either a excel file/directly to Google Sheets--

# list sheets
title <- "v1 Audio Audits D2D Partners Survey" 
tab <- "v1 audio audits"
tab_2 <- "quick check"
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
# gs_ws_new(audioAuditGSheet, ws_title = "mt_cars", input = head(mtcars))


# add data to the audio audit sheet--

# thresholdLengthOfSurvey_quickcheck <- 50
# quickCheck <- filter(wideDataColSubExpForAA_dateRange_random, durationOfSurveyInMinutes < thresholdLengthOfSurvey_quickcheck)
for(j in 1:nrow(finalDataCombRand)){
  gs_add_row(audioAuditGSheet, ws = tab, finalDataCombRand[j, (1:6)])
}

# 



