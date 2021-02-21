# Importing relevant libaries
library(tidyverse)
library(data.table)
library(readxl)
library(plyr)
library(stargazer)

# Location of files being imported
surveyForm_location <- "E:/Surveys/SurveyCTO/Enrollment survey D2D partners 20190414.xlsx"
surveyRawData_location <- "E:/Data/SurveyCTO/SurveyCTO Sync CSV/Enrollment survey D2D partners 20190223.csv"

# Path to output file with timestamp
outputFile_location <- paste("C:/Users/Satish/Desktop/Enrollment survey report ", Sys.Date(), ".docx", sep="")
# Creating a file object
outputFile <- file(outputFile_location)

# Reading relevant sheets from survey form
# Reading survey sheet
surveyForm_survey <- as.data.table(readxl::read_excel(surveyForm_location, sheet = "survey"))
surveyForm_choices <- as.data.table(readxl::read_excel(surveyForm_location, sheet = "choices"))

# Reading survey raw data from CSV file
surveyRawData_data <- data.table::fread(surveyRawData_location)

# Getting column names
column_names <- colnames(surveyRawData_data)

# Creating empty list for cleaned columns
lists_to_be_plotted <- list()

# Splitting the data.table into columns and cleaning each column.
# This is output as a list of vectors. Each vector is a list of finite values from each original column.
for (column in column_names)
{
  current_column <- surveyRawData_data[, ..column]
  current_column_clean_ind <- which(current_column!='')
  #lists_to_be_plotted[[length(lists_to_be_plotted) + 1]] <- unname(unlist(c(current_column[c(current_column_clean_ind)])))
  lists_to_be_plotted[[length(lists_to_be_plotted) + 1]] <- unname(unlist(c(current_column)))
}

# # Simple tables
# for (column_number in seq(1,length(lists_to_be_plotted)))
#     {
#     current_column <- lists_to_be_plotted[column_number]
#     if (length(unique(unlist(current_column))) < 15)
#         {
#         current_freq_table <- table(lists_to_be_plotted[column_number])
#         cat(column_names[column_number], "\n")
#         print(current_freq_table)
#         cat("\n")
#         cat("\n")
#         }
# }