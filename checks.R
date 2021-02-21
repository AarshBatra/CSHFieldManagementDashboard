## data checks: datamation survey---------

# NOTE: before runnnig this file check if the contents of the "getPartnerTeam" function needs to be updated in the 'helper.R' file


## meta data---------
# Author: Aarsh Batra
# Started on: March 30, 2019

## helper files--------
source("generate.R")

# Number of Surveys per surveyor on a certain date
date_input <- "2019-05-13"
rel_field <- "member_name"
indexes_date <- str_detect(ds_wide_expand$SubmissionDate, date_input)
ds_wide_expand_single_date <- ds_wide_expand[indexes_date, ]
ds_wide_expand_single_date <- arrange(ds_wide_expand_single_date, eval(as.name(rel_field)))
g_ds_wide_expand_single_date <- ggplot(ds_wide_expand_single_date) + geom_histogram(mapping = aes(x = eval(as.name(rel_field))), stat = "count") + coord_flip()
graphTitle <- str_c("Performance Graph, ", date_input)
g_ds_wide_expand_single_date + ggtitle(graphTitle)

# Number of surveys done per surveyor: time series graph (for a given surveyor name)
surveyorNameList <- unique(ds_wide_expand$member_name)
surveyorName <- "JYOTI"
ds_wide_expand_date_mem <- dplyr::group_by(ds_wide_expand, dateOnly, member_name)
ds_wide_expand_date_mem <- dplyr::summarize(ds_wide_expand_date_mem, count = n())
ds_wide_expand_date_mem <- dplyr::arrange(ds_wide_expand_date_mem, member_name)

ds_wide_expand_date_mem_SurvName <- dplyr::filter(ds_wide_expand_date_mem, member_name == surveyorName)
g_time_series_graph_mem <- ggplot(data = ds_wide_expand_date_mem_SurvName) + geom_line(mapping = aes(x = dateOnly, y = count, group = member_name))
# g_time_series_graph_mem <- g_time_series_graph_mem  + ggtitle(surveyorName)
# ggplotly(g_time_series_graph_mem)

# Number of surveys done per surveyor: time series graph (for all surveyors)
g_time_series_graph_all_survyeors <- ggplot(data = ds_wide_expand_date_mem) + geom_line(mapping = aes(x = dateOnly, y = count, 
                                                                                               group = member_name, colour = member_name)) + ggtitle("Surveyors Performance")
ggplotly(g_time_series_graph_all_survyeors)

# distribution checks: use the general 'plotDist' function from the helper file-------

# figuring out which variable do we need to see a distribution for


# summary tables for all the variables---------------------

## getting summary for R based variables-------------------

# linking the data description document with the 




# using "stargazer" package 
stargazer::stargazer(as.data.frame(ds_wide_expand), summary = TRUE, type = "html", out = "foo.html") 

# using "summarytools" package
# dfSummary()

# getting the number of surveys per surveyor since beginning of time. 
# Also getting the average number of surveys per day per surveyor since start.

# ds_wide_expand_perf_rev <- arrange(ds_wide_expand, dateOnly)
# ds_wide_expand_perf_rev_grp_mem_name <- group_by(ds_wide_expand_perf_rev, member_name)
# ds_wide_expand_perf_rev_summary <- dplyr::summarise(ds_wide_expand_perf_rev_grp_mem_name, numOfSurveys = n(), numDaysWithCSH = dateOnlyDateFormat[n()] - dateOnlyDateFormat[1])
# 
# 
# # average num of surveys overall (calculations for performance review)
# ds_wide_expand <- mutate(ds_wide_expand, dateOnlyDateFormat = as.Date(dateOnly))
# ds_wide_expand_group_mem <- arrange(ds_wide_expand, member_name)
# ds_wide_expand_group_mem <- group_by(ds_wide_expand_group_mem, member_name)
# ds_wide_expand_summary <- dplyr::summarise(ds_wide_expand_group_mem, numOfSurveys = n(), numDaysWithCSH = as.integer(dateOnlyDateFormat[n()] - dateOnlyDateFormat[1]), avgSurvDurInMin = mean(durationOfSurveyInMinutes, na.rm = TRUE))
# ds_wide_expand_summary <- mutate(ds_wide_expand_summary, numSurveysPerDay = numOfSurveys/numDaysWithCSH)
# mem_name_list_perf_rev <- c("babita srivastav", "Babita Srivastva", "JYOTI", "Kamalkumar",
#                             "Mahesh kumar",  "Mahesh Kumar", "Mohamad Azhar Ansari",
#                             "Mohammad Munaf", "Mohd irfan", "Pradeep Kumar Yadav", "Priya",
#                             "Rajeev gola", "Sunil", "Amitkumar Devidasji Aglawe")
# ds_wide_expand_summary <- filter(ds_wide_expand_summary, member_name %in% mem_name_list_perf_rev)
# ds_wide_expand_summary <- select(ds_wide_expand_summary, member_name, numOfSurveys, numDaysWithCSH, numSurveysPerDay, avgSurvDurInMin)
# write.csv(ds_wide_expand_summary, "perf_rev.csv")

# save all data-----------------
save(list = ls(all = TRUE), file= "all.RData")
