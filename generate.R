## generating new variables: datamation survey---------

## meta data---------
# Author: Aarsh Batra
# Started on: March 30, 2019

## helper files-----------
source("cleaning.R")

## generating relevant variables---------
ds_wide_expand <- ds_wide_clean

# column added: durationOfSurveyInMinutes----------
ds_wide_expand$SubmissionDate <- lubridate::mdy_hms(ds_wide_expand$SubmissionDate)
ds_wide_expand$starttime <- lubridate::mdy_hms(ds_wide_expand$starttime)
ds_wide_expand$endtime <- lubridate::mdy_hms(ds_wide_expand$endtime)

ds_wide_expand <- ds_wide_expand %>% mutate(durationOfSurveyInMinutes = interval(endtime, starttime))
ds_wide_expand$durationOfSurveyInMinutes <- as.duration(ds_wide_expand$durationOfSurveyInMinutes)
ds_wide_expand$durationOfSurveyInMinutes <- (abs(as.numeric(ds_wide_expand$durationOfSurveyInMinutes)))/60

# column added: dateOnly, startTimeOnly, endTimeOnly-----------
ds_wide_expand <- ds_wide_expand %>% mutate(dateOnly = str_extract(SubmissionDate, "....-..-.."), 
                                            startTimeOnly = str_extract(starttime, "..:..:.."), 
                                            endTimeOnly = str_extract(endtime, "..:..:.."))


# getting start time of first survey and start time for last survey for each surveyor
# for every date flag surveys if they the difference b/w these
# 2 times is less than 7 hours (420 minutes)

ds_wide_expand_sum_1 <- group_by(ds_wide_expand, member_name, starttime, endtime, dateOnly, durationOfSurveyInMinutes)
ds_wide_expand_sum_1 <- dplyr::summarize(ds_wide_expand_sum_1, count = n())
ds_wide_expand_sum_1 <- ds_wide_expand_sum_1 %>% group_by(dateOnly, member_name)
ds_wide_expand_sum_1  <- dplyr::summarize(ds_wide_expand_sum_1, stfs = starttime[1], 
                                     stls = starttime[n()], date = dateOnly[1], numSurveysPerDay = n())

ds_wide_expand_sum_1 <- arrange(ds_wide_expand_sum_1, member_name)

# getting the average number of surveys done per hour for each surveyor in a given day
ds_wide_expand_sum_1$durationInHrs <- (as.numeric(abs(as.duration(interval(ds_wide_expand_sum_1$stls, ds_wide_expand_sum_1$stfs)))))/3600
ds_wide_expand_sum_1$avgNumSurvPerHr <- (ds_wide_expand_sum_1$numSurveysPerDay)/ds_wide_expand_sum_1$durationInHrs

# flag surveys that whose avg num of surveys per hour is less than 3.5 hours or Num hours of work < 7 hours
db_flagSurveys_avgNumSurvPerHr <- filter(ds_wide_expand_sum_1, ((durationInHrs < 4) | (avgNumSurvPerHr < 3)))
db_flagSurveys_avgNumSurvPerHr <- arrange(db_flagSurveys_avgNumSurvPerHr, member_name)

# On a certain Date(s), the start and end time of the survey for each surveyor
ds_wide_expand_eachSurveyStEt <- group_by(ds_wide_expand, member_name, starttime, endtime, dateOnly, durationOfSurveyInMinutes)
ds_wide_expand_eachSurveyStEt <- dplyr::summarize(ds_wide_expand_eachSurveyStEt, count = n())
ds_wide_expand_eachSurveyStEt <- ds_wide_expand_eachSurveyStEt %>% mutate(startTimeOnly = str_extract(starttime, "..:..:.."), endTimeOnly = str_extract(endtime, "..:..:.."))

# write output excel files
# write.xlsx(dataSctoExp, "excelDashboard.xlsx", sheetName = "allData")
ds_wide_expand_eachSurveyStEt_exp <- select(ds_wide_expand_eachSurveyStEt, member_name, dateOnly, startTimeOnly, endTimeOnly, durationOfSurveyInMinutes)
write.xlsx(as.data.frame(ds_wide_expand_eachSurveyStEt_exp), file = "excelDashboard.xlsx", sheetName = "data_StEtEachSurvey")

ds_wide_expand_sum_1_exp <- select(ds_wide_expand_sum_1, member_name, dateOnly, stfs, stls, numSurveysPerDay, durationInHrs, avgNumSurvPerHr)
write.xlsx(as.data.frame(ds_wide_expand_sum_1_exp), file = "excelDashboard.xlsx", sheetName = "data_fsls_duration", append = TRUE)

db_flagSurveys_avgNumSurvPerHr_exp <- select(db_flagSurveys_avgNumSurvPerHr, member_name, dateOnly, stfs, stls, numSurveysPerDay, durationInHrs, avgNumSurvPerHr)
write.xlsx(as.data.frame(db_flagSurveys_avgNumSurvPerHr_exp), "excelDashboard.xlsx", sheetName = "flagSurveys_data_fsls_duration", append = TRUE)

# getting labelled columns list
source("20190424 - Generation - Enrollment survey D2D partners.R")

# getting the choices sheet from the Survey CTO form-------------
choicesSheet <- read_csv("Enrollment survey D2D partners 20190414 - choices.csv")
surveySheet <- read_csv("Enrollment survey D2D partners 20190414 - survey.csv")
surveySheet <- filter(surveySheet, !is.na(type))
View(surveySheet)
uniqFieldType <- unique(choicesSheet$list_name)
uniqFieldType <- uniqFieldType[!is.na(uniqFieldType)]

surveySheet$choices <- c(rep(NA, times = nrow(surveySheet)))
uniqTypeOnly <- c("text", "integer", "decimal", "select_one", "select_multiple", "geopoint", "geoshape", 
                  "geotrace", "barcode", "date", "datetime", "image", "audio", "video", "file",
                  "note", "start", "end", "deviceid", "subscriberid", "simserial", "caseid",
                  "phonenumber", "comments", "calculate", "calculate_here", "text audit",
                  "audio audit", "begin group", "end group", "begin repeat", "end repeat")

surveySheet$typeOnly <- c(rep(NA, times = nrow(surveySheet)))

for(i in 1: nrow(surveySheet)){
  for(j in 1: length(uniqFieldType)){
    tmpStrType <- surveySheet$type[i]
    tmpStrLog <- str_detect(tmpStrType, uniqFieldType[j])
    if(tmpStrLog == TRUE){
      surveySheet$choices[i] <- uniqFieldType[j]
    } else {
      next()
    }
  }
}

for(j in 1: nrow(surveySheet)){
  for(k in 1: length(uniqTypeOnly)){
    tmpStrType <- surveySheet$type[j]
    tmpStrLog <- str_detect(tmpStrType, uniqTypeOnly[k])
    if(tmpStrLog == TRUE){
      surveySheet$typeOnly[j] <- uniqTypeOnly[k]
    } else {
      next()
    }
  }
}


surveySheet <- select(surveySheet, type, typeOnly, choices, name, everything())


# getting only select_one columns and their corresponding indices
finalIndVec <- c(rep(NA, times = length(ds_wide_colnames)))
for(i in 1:length(ds_wide_colnames)){
  getIndVec <- str_detect(ds_wide_colnames, ds_wide_colnames[i])
  finalIndVec[i] <- which(getIndVec, arr.ind = TRUE)
}
df_selectOne <- tibble(name = ds_wide_colnames, index = finalIndVec)
df_selectOne <- left_join(df_selectOne, surveySheet, by = "name")
df_selectOne <- select(df_selectOne, name, typeOnly, index)
df_selectOne <- filter(df_selectOne, typeOnly == "select_one")

# getting the duration of the survey in minutes--------
durOfSurveyGrp <- dplyr::group_by(ds_wide_expand, member_name) 
durOfSurveyGrp_summ <- dplyr::summarise(durOfSurveyGrp, durOfSurInMinBySurveyor = mean(durationOfSurveyInMinutes))
durOfSurPerSurveyorGraph <- ggplot(durOfSurveyGrp_summ) + geom_point(mapping = aes(x = member_name, y = durOfSurInMinBySurveyor)) + coord_flip()

# # scale_x_discrete, coercing to factor variable, rename from the plyr library
# 
# counter <- 0
# ind <- 1
# choiceList <- list(NA)
# for(k in 1:nrow(choicesSheet)){
#   if(!is.na(choicesSheet$list_name[k])){
#     counter <- counter + 1
#   } else {
#     if(k == 1){
#       choiceList[[k]] <- choicesSheet[ind:counter, 1:3]
#       counter <- 0
#       ind <- 1 + k
#     } else {
#       choiceList[[k]] <- choicesSheet[(ind:(ind + counter)), 1:3]
#       counter <- 0
#       ind <- 1 + k
#     }
#   }
# }
# 
# choiceList <- choiceList[!is.na(choiceList)]
# 
# 
# ds_wide_expand_choices <- ds_wide_expand
# 
# for(i in 1:ncol(ds_wide_expand)){
#   tmpInd <- str_detect(surveySheet$name, colnames(ds_wide_expand[i]))
#   if(sum(tmpInd, na.rm = TRUE) == 0){
#     next
#   } else {
#     tmpChoice <- surveySheet$choices[tmpInd]
#     tmpChoice <- tmpChoice[!is.na(tmpChoice)]
#     for(j in 1:length(choiceList)){
#       if(is.null(choiceList[[j]])){
#         next
#       } else {
#         if(choiceList[[j]]$list_name[1] == tmpChoice){
#           for(k in 1:nrow(choiceList[[j]])){
#             tmpInd2 <- str_detect(ds_wide_expand_choices[, i], choiceList[[j]]$value[k])
#             ds_wide_expand_choices[tmpInd2, i] <- choiceList[[j]]$`label:english`[k]
#           }
#         }
#       }
#     }
#   }
# }
