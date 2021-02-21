# Getting Average time per question for v4 enrollment survey CSH

# library
library(tidyverse)
library(stringr)

# get the files--------------------------------------------------------------------------
taFiles <- list()
setwd("C:/Users/Aarsh/Desktop/media/Text Audits")
taFileNames <- list.files()
# taFileNames <- taFileNames[2:length(taFileNames)]
taFileNames[1] # sanity check
fieldsOverall <- c()

# getting field names across all text audio audit files and accumulating them------------
# in a single vector
# in a single vector
for(i in 1:length(taFileNames)){
  taFiles[[i]] <- read_csv(taFileNames[i])
  fieldsOverall <- append(fieldsOverall, taFiles[[i]]$`Field name`)
}

# getting only unique field names out of fieldsOverall vector
uniqueFieldsOverall <- unique(fieldsOverall)
length(uniqueFieldsOverall) # sanity check


# creating 2 'mxn' matrices where m = "Number of text audit files",---------------------- 
# n = "Number of unique field names across all text audit files"

indexes_mat <- matrix(data = NA, nrow = length(taFileNames), 
                      ncol = length(uniqueFieldsOverall))

indexes_mat_corresp_val <- matrix(data = NA, length(taFileNames), 
                                  ncol = length(uniqueFieldsOverall))


# filling in the matrices with the average values (non-vectorized solution)--------------
# Explanation: For each text audit file do the following:
#    1. Check which of the unique field names (stored in 'uniqueFieldsOverall') are
#       present in the current text audit file. So, a '0' in the indexes_mat[i, j]
#       means that for the in the i'th text audit file there is no field named 
#       uniqueFieldsOverall[j].
  
for(j in 1:length(taFileNames)){
  for (k in 1:length(uniqueFieldsOverall)){
    indexes_mat[j, k] <- uniqueFieldsOverall[k] %in% taFiles[[j]]$`Field name`
    if(indexes_mat[j, k] == TRUE){
      tmp <- filter(taFiles[[j]], `Field name` == uniqueFieldsOverall[k])
      indexes_mat_corresp_val[j, k] <- tmp$`Total duration (seconds)` 
    } else{
      indexes_mat_corresp_val[j, k] <- NA
    }
  }
}

colnames(indexes_mat_corresp_val) <- uniqueFieldsOverall

# assigning colnames to 'indexes_mat_corresp_val'
colnames(indexes_mat_corresp_val) <- uniqueFieldsOverall
indexes_mat_corresp_val <- as_tibble(indexes_mat_corresp_val)

# Getting average time per question
indexes_mat_corresp_val_avg <- matrix(data = NA, length(uniqueFieldsOverall), 2)
indexes_mat_corresp_val_avg[, 1] <- uniqueFieldsOverall
dummy_vec <- c(NA)
for(l in 1:length(uniqueFieldsOverall)){
  tmp2 <- uniqueFieldsOverall[l]
  indexes_mat_corresp_val_avg[l, 2] <- mean(indexes_mat_corresp_val[, l][[1]], na.rm = TRUE) 
}

indexes_mat_corresp_val_avg <- as_tibble(indexes_mat_corresp_val_avg)
colnames(indexes_mat_corresp_val_avg) <- c("QuestionNameUnrolled", "AverageTimeTakenToAdminQues")
indexes_mat_corresp_val_avg$AverageTimeTakenToAdminQues <- as.numeric(indexes_mat_corresp_val_avg$AverageTimeTakenToAdminQues)
View(indexes_mat_corresp_val_avg) # final result