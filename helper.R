# useful functions---------

# libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)
library(readxl)
library(xlsx)
library(plotly)
library(expss)

# making paths usable--------
correctPath <- function(){
  library(stringr)
  oldPath <- readline()
  newPath <- str_replace_all(oldPath, "\\\\", "/")
  setwd(newPath)
}

# get the column names for a given regular expression
str_get <- function(nameVec, regExp){
  tmp_ind <- stringr::str_detect(nameVec, regExp)
  return(nameVec[tmp_ind])
}

# plot distributions for a single variable
plotDist <- function(dataset, variable, baseR, geom, type){
  if(baseR == "no"){
    
    if(geom == "histogram"){
      
      if(type == "density"){
        tmp <- ggplot(dataset) + geom_histogram(mapping = aes(x = eval(as.name(variable)), y = ..count../sum(..count..))) 
        return(tmp)
      } else if (type == "count"){
        tmp <- ggplot(dataset) + geom_histogram(mapping = aes(x = eval(as.name(variable)))) 
        return(tmp)
      }
      
    } else if(geom == "bar"){
    
      if(type == "density"){
        tmp <- ggplot(dataset) + geom_bar(mapping = aes(x = eval(as.name(variable)), y = ..prop..))
        return(tmp)
      } else if(type == "count"){
        tmp <- ggplot(dataset) + geom_bar(mapping = aes(x = eval(as.name(variable))))
        return(tmp)
      }
      
    }
    
  } else { # use base R
    
    if(geom == "histogram"){
       
      if(type == "density"){
        
      } else if (type == "count"){
        
      }
    } else if(geom == "bar"){
      
      if(type == "density"){
        
      } else if (type == "count"){
        
      }
    } 
  }
}

# Evaluate a column name (second stage variables)
makeItVar <- function(var_name){
  return(eval(as.name(as.character(var_name))))
}


# get the index for the column name
getColInd <- function(ds, colName){
  if(is.null(dim(ds))){
    colNamesList <- ds
  } else {
    colNamesList <- colnames(ds) 
  }
  getLogInd <- str_detect(colNamesList, colName)
  getNumericInd <- which(getLogInd, arr.ind = TRUE)
  return(getNumericInd)
}


# plotting function version 2
plotDistV2 <- function(ds, listName, colName, geom){
  colNamesList <- colnames(ds)
  getIndVec <- str_detect(colNamesList, colName)
  getInd <- which(getIndVec, arr.ind = TRUE)
  if(geom == "Count"){
    ggplot(listName[[getInd]]) + geom_bar(mapping = aes(x = Choice, y = Count), stat = "identity") + coord_flip()
    
  } else if (geom == "Percentages"){
    ggplot(listName[[getInd]]) + geom_bar(mapping = aes(x = Choice, y = Percentages), stat = "identity") + coord_flip()
    
  }

}

# generating tables alongside the distributions
genTab <- function(ds, listName, colName){
  ds_sub_tmp <- filter(ds, name == colName)
  getInd <- ds_sub_tmp$index
  return(listName[[getInd]])
  # old implementation (v1)
  # colNamesList <- colnames(ds)
  # getIndVec <- str_detect(colNamesList, colName)
  # getInd <- which(getIndVec, arr.ind = TRUE)
  # return(listName[[getInd]])
}

# choiceListTab <- function(ds, listName, colName){
#   colNamesList <- colnames(ds)
#   getIndVec <- str_detect(colNamesList, colName)
#   getInd <- which(getIndVec, arr.ind = TRUE)
#   return(listName[[getInd]])
# }

choiceListTab <- function(ds, listName, colName){
  ds_sub_tmp <- filter(ds, name == colName)
  getInd <- ds_sub_tmp$index
  return(listName[[getInd]])
}

questionListTab <- function(ds, listName, colName, surveyForm){
  ds_sub_tmp <- filter(ds, name == colName)
  getInd <- ds_sub_tmp$index
# currentSurveyRow <- which(surveyForm[, name] == colNamesList[getInd])[1]
  return(listName[[getInd]])
  # # old implementation
  # colNamesList <- colnames(ds)
  # getIndVec <- str_detect(colNamesList, colName)
  # getInd <- which(getIndVec, arr.ind = TRUE)
  # currentSurveyRow <- which(surveyForm[, name] == colNamesList[getInd])[1]
  # return(listName[[getInd]])
}

# (this function does not work as of now) convert columns of type "character" to type "date" using the lubridate library's functions------
convertToTypeDate <- function(ds, colInd, charDateType){
  if(charDateType == "mdy"){
    tmpCol <- colnames(ds)[colInd]
    ds[, eval(tmpCol)] <- mdy(unlist(ds[, eval(tmpCol)]))
  } else if (charDateType == "dmy"){
    tmpCol <- colnames(ds)[colInd]
    ds[, eval(tmpCol)] <- dmy(unlist(ds[, eval(tmpCol)]))
  } else if(charDateType == "ymd"){
    tmpCol <- colnames(ds)[colInd]
    ds[, eval(tmpCol)] <- ymd(unlist(ds[, eval(tmpCol)]))
  }
}

# get team members list------------

getPartnerTeam <- function(partnerName){
  if(partnerName == "IDI"){
    teamMembers <- c("Mahendra", "Pradeep kumar mishra", "Rajan singh", "Suchita tiwari", "Sangeeta Devi", "BRAHM DEV VERMA", "Sonu verma")
    return(teamMembers)
  } else if(partnerName == "Datamation"){
    teamMembers <- c("Ashutosh kumar dubey", "Amita Shukla", "Vinay Mishra", "Jasvant Kumar", "Reetesh kumar", "Jyoti Dwivedi", "Sanya", "Vikash Panday", "Ankit Tripathi", "RAJESH KUMAR singh", "Balram yadav", "Vikash pandey", "Som Trivedi", "HINA sharma", "Neelma sharma", "Shyam Singh", "Ankit Tripathi", "Sanya")
    return(teamMembers)
  } else if(partnerName == "HospitalWorkers"){
    teamMembers <- c("Mohammad Munaf", "Sunil", "Amitkumar Devidasji Aglawe", "Amitkumar D. Aglawe", "Rajshree Raman", "Nidhi Baghel", "Swati Bhadkariya")
    return(teamMembers)
  } else if(partnerName == "unknown"){
    teamMembers <- c("Imtiyaz")
    return(teamMembers)
  } else if(partnerName == "d2dinhouse"){
    teamMembers <- c("Arjun", "JYOTI", "Mohamad Azhar Ansari", "Rajeev gola", "Mahesh kumar", "Mahesh Kumar", "Babita srivastva", "Mohd irfan", "Priya", "Pradeep Kumar Yadav", "Rajkumar singh", "Unique", "Mahesh Kumar", "GAURAV KUMAR", "Ved prakash", "Pankaj", 
                     "Kamalkumar", "Imtiyaz", "Babita Srivastva", "Babita srivastva")
    return(teamMembers)
  }
}



