## Receiving and Understanding Survey Analysis--------------


# source files---------
source("checks.R")


# get data---------
ru_data_raw <- read_csv("Receiving and Understanding Survey_WIDE.csv")
ru_scto_choices_sheet <- read_csv("Receiving and Understanding Survey AB v1 - choices.csv")


# relevant columns for analysis----------
rel_col <- c("person_unavail", "rel_with_mother", "rec_sms_from_CSH_flag", 
                         "can_rem_msg_content_flag", "msg_content", "delete_sms_flag",
                         "check_inbox", "read_msg_content_flag", "read_msg_content")

ru_data_raw_rel_col <- select(ru_data_raw, rel_col)

# Plotting relevant columns-------

plotList <- list()

for(i in 1:length(rel_col)){
  plotList[[i]] <- ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[i])), y = ..count../sum(..count..))) + ggtitle(rel_col[i])
}

g1 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[1])), y = ..count../sum(..count..))) + ggtitle(rel_col[1]))
g2 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[2])), y = ..count../sum(..count..))) + ggtitle(rel_col[2]))
g3 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[3])), y = ..count../sum(..count..))) + ggtitle(rel_col[3]))
g4 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[4])), y = ..count../sum(..count..))) + ggtitle(rel_col[4]))
g5 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[5])), y = ..count../sum(..count..))) + ggtitle(rel_col[5]))
g6 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[6])), y = ..count../sum(..count..))) + ggtitle(rel_col[6]))
g7 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[7])), y = ..count../sum(..count..))) + ggtitle(rel_col[7]))
g8 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[8])), y = ..count../sum(..count..))) + ggtitle(rel_col[8]))
g9 <- ggplotly(ggplot(ru_data_raw) + geom_bar(mapping = aes(x = eval(as.name(rel_col[9])), y = ..count../sum(..count..))) + ggtitle(rel_col[9]))

# summary table for figuring out 'n'----------
stargazer(as.data.frame(ru_data_raw_rel_col), type = "text") 

# Who is the respondent who will call?----------
percTimesWrongNumber <- count_if(2, ru_data_raw$is_resp_cont_name)/length(ru_data_raw$is_resp_cont_name)
percTimesRespContName <- count_if(1, ru_data_raw$is_resp_cont_name)/length(ru_data_raw$is_resp_cont_name)
percTimesRespNotContName <- 1 - (percTimesWrongNumber + percTimesRespContName)  

# Of the ones who say "they did not receive an SMS", what % found a message in the inbox on checking the content--------
nrow(filter(ru_data_raw_rel_col, rec_sms_from_CSH_flag == 2, check_inbox == 1))
