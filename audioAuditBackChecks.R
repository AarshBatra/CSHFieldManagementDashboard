## audio audit back check code-----------

# get enrollment survey processed data----------
source("checks.R")

# read Audio audit data----------
audioAuditData <- readr::read_csv("v1 Audio Audits D2D Partners Survey - v3 audio audits.csv")
# audioAuditData <- readxl::read_xlsx("v1 Audio Audits D2D Partners Survey.xlsx")

# cleaning data-------------
audioAuditDataClean <- audioAuditData[(9:nrow(audioAuditData)), ]
colnames(audioAuditDataClean) <- as.character(audioAuditDataClean[1, ])
audioAuditDataClean <- audioAuditDataClean[(2:nrow(audioAuditDataClean)), ]
View(audioAuditDataClean[!is.na(audioAuditDataClean), ])

# relevant columns to be backchecked:
relCol <- c("mothers_name", "child1s_name", "fathers_name", "fathers_contact_number", "child1s_dob")

# create 2 dataframes, one for audio audit data and one for the enrollment survey data
relRowsKeys <- select(audioAuditDataClean, "KEY")
relRowsKeys <- relRowsKeys$KEY

df_enrSur <- filter(ds_wide_expand, KEY %in% relRowsKeys)
df_enrSur <- select(df_enrSur, KEY, relCol)  
df_enrSur <- df_enrSur[!is.na(df_enrSur$KEY), ]
df_enrSur <- arrange(df_enrSur, KEY)

df_aa <- select(audioAuditDataClean, KEY, relCol)
df_aa <- df_aa[!is.na(df_aa$KEY), ]
df_aa <- arrange(df_aa, KEY)

# Checking father's contact number in the 2 datasets------
phnNumComp <- (df_aa$fathers_contact_number == df_enrSur$fathers_contact_number)
phnNumComp_nonNA <- phnNumComp[!is.na(phnNumComp)]
(sum(phnNumComp, na.rm = TRUE))/(sum(phnNumComp_nonNA)) # 100% non NA numbers match

# Comparing child1s dob in two sections
ch1DOBIndex_fullDateOnly <- str_detect(df_aa$child1s_dob, "2018|2019") # getting only 'dates'
ch1DOB_fullDateOnly <- df_aa$child1s_dob[ch1DOBIndex_fullDateOnly]

ch1DOBComp <- (df_aa$child1s_dob[ch1DOBIndex_fullDateOnly] == df_enrSur$child1s_dob[ch1DOBIndex_fullDateOnly])
sum(ch1DOBComp, na.rm = TRUE) # 100% match in child's dob

# Comparing mother's name-------
ch1MothersName <- (df_aa$mothers_name == df_enrSur$mothers_name) 
sum(ch1MothersName, na.rm = TRUE) # not useful

# # distribution checks---------
# ggplot(audioAuditDataClean) + geom_bar(mapping = aes(x = "Asked for Consent", y = ..count../sum(..count..)))
# ggplot(audioAuditDataClean) + geom_bar(mapping = aes(x = "Asked for Consent Signature", y = ..count../sum(..count..)))
# ggplot(audioAuditDataClean) + geom_bar(mapping = aes(x = "Respondent Phone Type", y = ..count../sum(..count..)))

# Getting the automatic score and flagging those surveys for which the score is less than a particular threshold
autoScoreThreshold <- 1.5
colnames(audioAuditDataClean)[ncol(audioAuditDataClean) - 2] <- "AutomaticScore" 
audioAuditDataClean[, "AutomaticScore"] <- as.numeric(unlist(audioAuditDataClean[, "AutomaticScore"]))
dataSubsetAutoScore <- audioAuditDataClean[(audioAuditDataClean$AutomaticScore < 1.5), ]
colnames(dataSubsetAutoScore)[90] <- "C90"
dataSubsetAutoScore <- filter(dataSubsetAutoScore, !is.na(KEY)) # look into these surveys
write.csv(dataSubsetAutoScore, "audioAuditOutputFiles/audioAuditsFlaggedGivenAutomaticScore_20190430.csv")

View(df_aa)




