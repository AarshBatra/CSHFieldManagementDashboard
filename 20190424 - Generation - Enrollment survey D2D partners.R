# Importing relevant libraries
library(tidyverse)
library(data.table)
library(readxl)
library(plyr)
library(stargazer)

# Location of files being imported
surveyForm_location <- "C:/Users/Aarsh/Dropbox/CSH/FieldManagementDashboard/Enrollment survey D2D partners 20190414.xlsx"
surveyRawData_location <- "C:/Users/Aarsh/Dropbox/CSH/FieldManagementDashboard/Enrollment survey D2D partners 20190223_WIDE.csv"

# Path to output file with timestamp
outputFile_location <- paste("C:/Users/Aarsh/Dropbox/CSH/FieldManagementDashboard/Enrollment survey D2D partners 20190223_WIDE.csv", Sys.Date(), ".docx", sep="")
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




freqTableList <- list(rep(NA, times = length(lists_to_be_plotted)))
choiceListOut <- list(rep(NA, times = length(lists_to_be_plotted)))
questionListOut <- list(rep(NA, times = length(lists_to_be_plotted)))
# questionListOut <- list(rep(NA, times = length(lists_to_be_plotted)))
# Tables that take into account type of form field
for (column_number in seq(1,length(lists_to_be_plotted)))
{
  cat("Column Number: ", column_number, "\n")
  if (length(which(surveyForm_survey[,name] == column_names[column_number])) != 0)
  {
    current_survey_row <- which(surveyForm_survey[,name] == column_names[column_number])[1]
    
    if (str_detect(surveyForm_survey[current_survey_row,1], 'select_one'))
    {
      current_survey_choiceList <- as.character(as.list(strsplit(as.character(surveyForm_survey[current_survey_row,1]), " ")[[1]])[2])
      current_survey_choiceTable <- surveyForm_choices[list_name == current_survey_choiceList]
      current_freq_table <- table(lists_to_be_plotted[column_number])
      
      # Printing column name
      cat("FIELD NAME: ")
      cat(column_names[column_number],"\n\n")
      # Writing to output file
      #write(c("FIELD NAME: ", column_names[column_number],"\n\n"), outputFile, append=TRUE)
      #print(c("FIELD NAME: ", column_names[column_number],"\n\n"), target = outputFile_location)
      
      # Printing field label
      cat("LABEL / QUESTION: ", "\n")
      cat(unlist(surveyForm_survey[current_survey_row,3]),"\n\n")
      questionListOut[[column_number]] <- unlist(surveyForm_survey[current_survey_row, 3])
      
      # Printing current choice list used
      cat("CHOICE LIST: ")
      stargazer(as.data.frame(current_survey_choiceTable)[, "label:english"], column.sep.width="40pt", flip=TRUE, summary=FALSE, type="text", colnames = FALSE, rownames = FALSE)
      choiceListOut[[column_number]] <- current_survey_choiceTable[, "label:english"]
      #stargazer(as.data.frame(current_survey_choiceTable)[, "label:english"], column.sep.width="40pt", flip=TRUE, summary=FALSE, type="html", colnames = FALSE, rownames = FALSE, out = outputFile_location)
      cat("\n")
      
      for (choice_number in seq(1,dim(current_survey_choiceTable)[1]))
      {
        #cat("choice number = ", choice_number, "\n")
        if (length(which(as.list(names(current_freq_table)) == as.character(as.integer(unlist(current_survey_choiceTable[choice_number,2]))))) != 0)
        {
          #current_freq_table <- plyr::rename(current_freq_table, replace = c(as.character(current_survey_choiceTable[choice_number,2]) = as.character(current_survey_choiceTable[choice_number,3])))
          
          names(current_freq_table) <- sub(as.character(as.integer(current_survey_choiceTable[choice_number,2])), as.character(current_survey_choiceTable[choice_number,3]), names(current_freq_table))
          
        }
      }
      
      #cat("\n", "FREQUENCY TABLE: ")
      if (dim(current_freq_table)[1] != 0)
      {
        
        current_freq_table <- as.data.frame(current_freq_table)
        current_freq_table["Percentages"] <- signif(current_freq_table[,2]*100/sum(current_freq_table[,2]), digits = 2)
        names(current_freq_table)[1] <- "Choice"
        names(current_freq_table)[2] <- "Count"
        
        ## Aarsh 
        current_freq_table["Total Observations"] <- sum(current_freq_table[, 2])
        
        cat("Number of observations : ", sum(current_freq_table[,2]))
        
        stargazer(as.data.frame(current_freq_table), summary=FALSE, type="text", rownames = FALSE)
        freqTableList[[column_number]] <- as.data.frame(current_freq_table)
        #stargazer(as.data.frame(current_freq_table), summary=FALSE, type="html", colnames = FALSE, rownames = FALSE, out = outputFile_location)
      }
      else
      {
        cat("Table has 0 rows probably because the question has not been accessed in the survey.")
      }
      cat("\n\n\n\n")
    }
  }
}

# Closing output file
close(outputFile)
