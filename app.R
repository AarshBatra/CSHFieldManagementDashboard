# shiny dashboard

# NOTE: before running this file check if the "getPartnerTeam" function's contents need to be updated in the 'helper.R' file.

# helper files
# source("fieldMonitoringDashboard.R")
## app.R ##
library(shiny)
library(shinythemes)
library(plotly)
library(graphics)
library(stats)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(base)
library(tidyr)
library(purrr)
library(rsconnect)
library(lubridate)
library(xlsxjars)
library(rJava)


# save(list = ls(all.names = TRUE), file = "#fullEnvironToRunAllCode.RData", envir = .GlobalEnv, compress = FALSE)
# save(list = ls(all = TRUE), file= "all.RData") 

# load(file = "#fullEnvironToRunAllCode.RData")
load("all.RData", .GlobalEnv) # NOTE: Always use load("fileName.RData", .GlobalEnv) rather than sourcing files, this will help in avoiding online deployment to shinyapps.io




ui <- fluidPage(
  theme = shinytheme("spacelab"),
  navbarPage(
    "Field Manangement Dashboard",
    navbarMenu(
      "Daily Dashboard",
      tabPanel(
        "Individual Performance",
        fluidRow(
          column(4,
                   dateInput("sd", "Date", today() - 1)
          ), 
          column(4, 
                selectInput("partnerName", "Partners", choices = c("d2dinhouse", "IDI", "HospitalWorkers", "Datamation", "unknown")) 
          )
        ),
        hr(),
        fluidRow(
          column(4, 
                 wellPanel(
                   plotlyOutput("numOfSurveys_daily")
                 )
          ),
          column(4, 
                 wellPanel(
                   plotlyOutput("durationOfDay_daily")
                 )
          ), 
          column(4, 
                 wellPanel(
                   plotlyOutput("avgNumSurPerHour")
                 )
          )
        ),
        fluidRow(
          plotlyOutput("gapsBetSur_daily")
        )
      ),
      tabPanel(
        "Distributions",
        fluidRow(
          column(6,
                 wellPanel(
                   plotlyOutput("durationOfSurvey_daily")
                 )
          )
      )
      ),
      tabPanel(
        "Other important graphs", 
        plotlyOutput("durOfSurvPerSurveyor")
      )
    ),
    navbarMenu(
      "Monthly Dashboard",
      tabPanel(
        "Individual Performance",
        fluidRow(
          column(12,
                 wellPanel(
                   sidebarLayout(
                     sidebarPanel(
                       selectInput("cshEmpNames", "Employee Name", choices = c(as.character(surveyorNameList), "allInOne"), selected = surveyorNameList[1]),
                       br(),
                       shiny::tags$small("Hover mouse pointer above the graph.")
                     ),
                     mainPanel(
                       plotlyOutput("surveyorPerfOverTime_monthly")
                     )
                   )
                 )
          )
        )
      ),
      tabPanel(
        "Distributions",
        fluidRow(
          column(12, 
              wellPanel(
                 sidebarLayout(
                   sidebarPanel(
                     h3("Distribution Parameters"),
                     br(),
                     dateInput("date_range_ll_1", "Date (From)", today() - 1),
                     dateInput("date_range_ul_1", "Date (To)", today()),
                     br(),
                     selectInput("rawVarList_1", "Variable List", ds_wide_colnames, selected = ds_wide_colnames[1]),
                     br(),
                     selectInput("typeOfGraph_1", "Graph Type", c("bar", "histogram"), "bar"),
                     br(),
                     selectInput("statType_1", "Stat Type", c("count", "density"), "count")
                   ), mainPanel(
                     plotlyOutput("dist_output_1")
                   )
                 )
         )
        ),
         fluidRow(
           column(12,
                  wellPanel(
                    sidebarLayout(
                      sidebarPanel(
                        h3("Distribution Parameters"),
                        br(),
                        dateInput("date_range_ll_2", "Date (From)", today() - 1),
                        dateInput("date_range_ul_2", "Date (To)", today()),
                        br(),
                        selectInput("rawVarList_2", "Variable List", ds_wide_colnames, selected = ds_wide_colnames[1]),
                        br(),
                        selectInput("typeOfGraph_2", "Graph Type", c("bar", "histogram"), "bar"),
                        br(),
                        selectInput("statType_2", "Stat Type", c("count", "density"), "count")
                      ), mainPanel(
                        plotlyOutput("dist_output_2")
                      )
                    )
                  )
          )
         )
        )
      ),
      tabPanel(
        "Distributions with Labels",
        sidebarLayout(
          sidebarPanel(
            selectInput("rawVarList_lab", "Variable List", df_selectOne$name, selected = df_selectOne$name[1]),
            br(),
            selectInput("geom", "Graph Type", c("Count", "Percentages"), selected = "Count"),
            br()
          ),
          mainPanel(
            plotlyOutput("dist_output_lab_1"), 
            br(),
            br(),
            h4("Question:"),
            br(),
            tableOutput("questionOutput"),
            h4("Summary Statistics"),
            tableOutput("summaryTableOutput"),
            br(),
            h4("Choices List:"),
            br(),
            tableOutput("choiceListPerColumn")
          )
        )
      ),
      # tabPanel(  # inactive for now, to activate this uncomment it here and in server code.
      #   "Tables",
      #   dataTableOutput("table_1")
      # ),
      tabPanel(
        "Other important graphs"
      )
    )
  )
)

server <- function(input, output, session){
  
  # ------------------------daily dashboard code------------------------
  
  output$numOfSurveys_daily <- renderPlotly({
    db_indexes_date_sd <- str_detect(ds_wide_expand$SubmissionDate, as.character(input$sd))
    db_ds_wide_expand_sd <- ds_wide_expand[db_indexes_date_sd, ]
    db_ds_wide_expand_sd <- arrange(db_ds_wide_expand_sd, member_name)
    db_ds_wide_expand_sd <- filter(db_ds_wide_expand_sd, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t1_g1 <- ggplot(db_ds_wide_expand_sd) + geom_histogram(mapping = aes(x = member_name), stat = "count") + coord_flip() + ggtitle(str_c("Number of Surveys on:", as.character(input$sd), sep = " ")) + scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50))
    ggplotly(db_daily_t1_g1)
  })
  
  output$durationOfSurvey_daily <- renderPlotly({
    db_daily_durSurvey_ds <- filter(ds_wide_expand_eachSurveyStEt, dateOnly == as.character(input$sd))
    db_daily_durSurvey_ds <- filter(db_daily_durSurvey_ds, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t3_g1 <- ggplot(db_daily_durSurvey_ds) + geom_histogram(mapping = aes(x = durationOfSurveyInMinutes, y = ..count../sum(..count..)), bins = 200) + ggtitle(str_c("Distribution: Duration of Survey (in minutes): ", as.character(input$sd), sep = " "))
    ggplotly(db_daily_t3_g1)
    
  })
  
  output$durationOfDay_daily <- renderPlotly({
    db_durOfDayForSurv_ds <- dplyr::filter(ds_wide_expand_sum_1, dateOnly == as.character(input$sd))
    db_durOfDayForSurv_ds <- dplyr::filter(db_durOfDayForSurv_ds, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t1_g2 <- ggplot(db_durOfDayForSurv_ds) + geom_point(mapping = aes(x = member_name, y = durationInHrs)) + coord_flip() + ggtitle(str_c("Duration of Work Day: ", as.character(input$sd), sep = " "))
    ggplotly(db_daily_t1_g2)
    
  })
  
  output$avgNumSurPerHour <- renderPlotly({
    db_avgNumSurPerHour_ds <- dplyr::filter(ds_wide_expand_sum_1, dateOnly == as.character(input$sd))
    db_avgNumSurPerHour_ds <- dplyr::filter(db_avgNumSurPerHour_ds, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t1_g3 <- ggplot(db_avgNumSurPerHour_ds) + geom_point(mapping = aes(x = member_name, y = avgNumSurvPerHr)) + coord_flip() + ggtitle(str_c("Avg Num of Surveys per hr: ", as.character(input$sd), sep = " "))
    ggplotly(db_daily_t1_g3)
  })
  
    output$durOfSurvPerSurveyor <- renderPlotly({
    db_indexes_date_sd <- str_detect(ds_wide_expand$SubmissionDate, as.character(input$sd))
    db_ds_wide_expand_sd <- ds_wide_expand[db_indexes_date_sd, ]
    db_ds_wide_expand_sd_grp <- group_by(db_ds_wide_expand_sd, member_name)
    db_ds_wide_expand_sd_summ <- dplyr::summarise(db_ds_wide_expand_sd_grp, durOfSurvPerSurveyor = mean(durationOfSurveyInMinutes, na.rm = TRUE))
    db_ds_wide_expand_sd_summ <- dplyr::filter(db_ds_wide_expand_sd_summ, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t1_g4 <- ggplot(db_ds_wide_expand_sd_summ) + geom_point(mapping = aes(x = member_name, y = durOfSurvPerSurveyor)) + coord_flip() + ggtitle(str_c("Average Survey duration: ", as.character(input$sd), sep = " "))
    ggplotly(db_daily_t1_g4)
  })
  
  output$gapsBetSur_daily <- renderPlotly({
    db_gapsBetSur_ds <- filter(ds_wide_expand_eachSurveyStEt, dateOnly == as.character(input$sd))
    db_gapsBetSur_ds <- filter(db_gapsBetSur_ds, member_name %in% (getPartnerTeam(input$partnerName)))
    db_daily_t1_g5 <- ggplot(db_gapsBetSur_ds) + geom_point(mapping = aes(x = starttime, y = member_name)) + ggtitle(str_c("Gaps between surveys: ", as.character(input$sd), sep = " ")) + scale_x_datetime(date_breaks = "1 hours")
    ggplotly(db_daily_t1_g5)
    
  })
  
  #-------------------------weekly dashboard code------------------------
  
  
  
  
  
  
  
  # -----------------------monthly dashboard code------------------------
  
  output$surveyorPerfOverTime_monthly <- renderPlotly({
    if(input$cshEmpNames != "allInOne"){
      db_surveysOverTime <- dplyr::filter(ds_wide_expand_date_mem, member_name == input$cshEmpNames)
      db_monthly_t1_g1 <- ggplot(data = db_surveysOverTime) + geom_line(mapping = aes(x = dateOnly, y = count, group = member_name))
      db_monthly_t1_g1 <- db_monthly_t1_g1 + ggtitle(input$cshEmpNames) +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
      ggplotly(db_monthly_t1_g1)
    } else {
      db_monthly_t1_g1 <- ggplot(data = ds_wide_expand_date_mem) + geom_line(mapping = aes(x = dateOnly, y = count, group = member_name, colour = member_name)) +  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
      ggplotly(db_monthly_t1_g1)
    }
    
  })
  
  output$dist_output_1 <- renderPlotly({
    db_ds_wide_expand_date_range_1 <- ds_wide_expand[((ds_wide_expand$dateOnly >= input$date_range_ll_1) & (ds_wide_expand$dateOnly <= input$date_range_ul_1)), ]
    db_monthly_t3_g1 <- plotDist(db_ds_wide_expand_date_range_1, input$rawVarList_1, "no", input$typeOfGraph_1, input$statType_1)
  })
  
  
  output$dist_output_2 <- renderPlotly({
    db_ds_wide_expand_date_range_2 <- ds_wide_expand[((ds_wide_expand$dateOnly >= input$date_range_ll_2) & (ds_wide_expand$dateOnly <= input$date_range_ul_2)), ]
    db_monthly_t3_g2 <- plotDist(db_ds_wide_expand_date_range_2, input$rawVarList_2, "no", input$typeOfGraph_2, input$statType_2)
  })
  
  # output$table_1 <- renderDataTable(ds_wide_expand) (inactive for now, to activate this, uncomment it here and also uncomment the corresponding lines of code in the ui script)
  
  output$dist_output_lab_1 <- renderPlotly({
    tmpPlt <- plotDistV2(ds_wide_expand, freqTableList, input$rawVarList_lab, input$geom)
    ggplotly(tmpPlt)
  })
  
  output$summaryTableOutput <- renderTable(
    genTab(df_selectOne, freqTableList, input$rawVarList_lab)
  )

  output$choiceListPerColumn <- renderTable(
    choiceListTab(df_selectOne,  choiceListOut, input$rawVarList_lab)
  )
  
  output$questionOutput <- renderTable(
    questionListTab(df_selectOne, questionListOut, input$rawVarList_lab, surveyForm_survey)
  )
  
  
## Old Code (Do not use)----------------------------------------------------------------------------------  
  #    output$dist_output <- renderPlotly({
  #    tmp_ds <- filter(distDatabase, dropdownName == input$distributions)
  #      if(tmp_ds$source == "notR"){
  # 
  #        if(tmp_ds$Standalone == 1){
  #          if(tmp_ds$graphType == "barPerc"){
  #            tmp_gr <- ggplot(chLevelData) + geom_bar(mapping = aes(x = eval(as.name(tmp_ds$f1[!is.na(tmp_ds$f1)])), y = ..prop..)) + xlab(tmp_ds$dropDownName)
  #            return(ggplotly(tmp_gr))
  #          } else if(tmp_ds$graphType == "freqpoly"){
  #            tmp_gr <- ggplot(chLevelData) + geom_freqpoly(mapping = aes(x = eval(as.name(tmp_ds$f1[!is.na(tmp_ds$f1)])))) + xlab(tmp_ds$dropDownName)
  #            return(ggplotly(tmp_gr))
  #          }
  # 
  #        } else { # not R and not standalone
  #          if(tmp_ds$graphType == "barPerc"){
  #            
  #          } else if(tmp_ds$graphType == "freqpoly"){
  #            v4v5_ss <-  as.data.frame(select(chLevelData, tmp_ds$f1))
  #            v4v5_ss <- filter(v4v5_ss, eval(as.name(tmp_ds$f1)) > 0)
  #            tmp_gr <- ggplot(v4v5_ss) + geom_freqpoly(mapping = aes(x = eval(as.name(tmp_ds$f1)))) + geom_vline(mapping = aes(xintercept = median(eval(as.name(tmp_ds$f1)), na.rm = TRUE)))
  #            
  #          }
  #        }
  # 
  #      } else { # if R
  # 
  #     if(tmp_ds$Standalone == 1){
  #       if(tmp_ds$graphType == "barPerc"){
  #        tmp_gr <-  ggplot(v4v5combined_scg) + geom_bar(mapping = aes(x = eval(as.name(tmp_ds$f1[!is.na(tmp_ds$f1)])), y = ..prop..)) + xlab(tmp_ds$dropDownName)
  #        return(ggplotly(tmp_gr))
  #       } else if(tmp_ds$graphType == "freqpoly"){
  #         tmp_gr <-  ggplot(v4v5combined_scg) + geom_freqpoly(mapping = aes(x = eval(as.name(tmp_ds$f1[!is.na(tmp_ds$f1)])))) + xlab(tmp_ds$dropDownName)
  #         return(ggplotly(tmp_gr))
  #       }
  # 
  #     } else {
  #       if(tmp_ds$graphType == "barPerc"){
  #         tmp_ds_f1_dat <- select(v4v5combined_scg, tmp_ds$f1)
  #         tmp_ds_f2_dat  <- select(v4v5combined_scg, tmp_ds$f2)
  #         tmp_ds_comb <- unlist(append(tmp_ds_f1_dat, tmp_ds_f2_dat))
  #         tmp_ds_comb <- tmp_ds_comb[!is.na(tmp_ds_comb)]
  #         tmp_gr <- ggplot(as.data.frame(tmp_ds_comb)) + geom_bar(mapping = aes(x = tmp_ds_comb, y = ..prop..)) + xlab(tmp_ds$dropDownName)
  #         return(ggplotly(tmp_gr))
  #      } else if(tmp_ds$graphType == "freqpoly"){
  #         tmp_ds_f1_dat <- select(v4v5combined_scg, tmp_ds$f1)
  #         tmp_ds_f2_dat  <- select(v4v5combined_scg, tmp_ds$f2)
  #         tmp_ds_comb <- unlist(append(tmp_ds_f1_dat, tmp_ds_f2_dat))
  #         tmp_ds_comb <- tmp_ds_comb[!is.na(tmp_ds_comb)]
  #         tmp_gr <- ggplot(as.data.frame(tmp_ds_comb)) + geom_freqpoly(mapping = aes(x = tmp_ds_comb)) + xlab(tmp_ds$dropDownName)
  #         return(ggplotly(tmp_gr))
  # 
  #       }
  #     }
  #   }
  # })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # output$dist_output <- renderPlotly({
  #   if(input$distributions == "bcg"){
  #     tmp <- ggplot(chLevelData) + geom_freqpoly(mapping = aes(x = bcg_overdueby))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "penta1"){
  #     tmp <- ggplot(chLevelData) + geom_freqpoly(mapping = aes(x = penta1_overdueby))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "mr1"){
  #     tmp <- ggplot(chLevelData) + geom_freqpoly(mapping = aes(x = mr1_overdueby))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "literacy"){
  #     tmp <- ggplot(chLevelData) + geom_bar(mapping = aes(x = literacy_1, y = ..prop..))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "vaccAvail"){
  #     tmp <- ggplot(chLevelData) + geom_bar(mapping = aes(x = as.integer(vac_card_avail), y = ..prop..), binwidth = 0.5)
  #     ggplotly(tmp)
  #   } else if(input$distributions == "child age"){
  #     tmp <- ggplot(chLevelData) + geom_freqpoly(mapping = aes(x = child_age))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "hh has eligible child"){
  #     tmp <- ggplot(v4v5combined_scg) + geom_bar(mapping = aes(x = hh_has_elg_ch, y = ..prop))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "hh has pregnant women"){
  #     tmp <- ggplot(v4v5combined_scg) + geom_bar(mapping = aes(x = hh_has_preg_wmn, y = ..prop..))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "number of children per caregiver"){
  #     tmp <- ggplot(v4v5combined_scg) + geom_bar(mapping = aes(x = curr_cg_elg_ch_qnt_1, y = ..prop..))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "like to receive SMS reminders"){
  #     tmp <- ggplot(v4v5combined_scg) + geom_bar(mapping = aes(x = like_rec_sms_rem_1, y = ..prop..))
  #     ggplotly(tmp)
  #   } else if(input$distributions == "have phone number"){
  #     tmp_data <- append(v4v5combined_scg$have_phone_number_1_1, v4v5combined_scg$have_phone_number_mf_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("have phone number")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Phone number main verify"){
  #     tmp_data <- append(v4v5combined_scg$phn_num_main_vrfy_1_1, v4v5combined_scg$ph_num_main_ver_mf_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("have phone number")
  #     tmp <- ggplot(v4v5combined_scg) + geom_histogram(mapping = aes(x = have_phone_number_1_1), binwidth = 0.5)
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Is main phone smart phone"){
  #     tmp_data <- append(v4v5combined_scg$is_main_phn_smart_phone_1_1, v4v5combined_scg$is_main_phn_smart_phone_mf_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Is main phone smart phone")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "have alternate phone number"){
  #     tmp_data <- append(v4v5combined_scg$hv_alt_phn_num_1_1, v4v5combined_scg$have_alt_ph_num_mf_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Have alternate phone number")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Alternate phone number relationship with child"){
  #     tmp_data <- append(v4v5combined_scg$alt_ph_num_own_rel_wth_ch_1_1_1, v4v5combined_scg$alt_ph_num_own_rel_ch_mf_1_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Alternate phone number owner relationship with child")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "sms language preference"){
  #     tmp_data <- append(v4v5combined_scg$sms_language_preference_1_1, v4v5combined_scg$sms_language_preference_mf_1)
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("SMS Language Preference")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "vaccination card photo permission"){
  #     tmp_data <- v4v5combined_scg$vc_crd_photo_perm_1_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Vaccination Card Photo Permission")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "vaccination card missing recall"){
  #     tmp_data <- v4v5combined_scg$vc_crd_miss_recall_1_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Vaccination Card Missing Recall")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "have MCP card or not"){
  #     tmp_data <- v4v5combined_scg$have_MCP_card_or_not_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("have MCP card or not")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "MCP card due date on card"){
  #     tmp_data <- v4v5combined_scg$MCP_card_due_date_on_card_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("MCP card due date on card")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "First TT Injection"){
  #     tmp_data <- v4v5combined_scg$first_tt_injection_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("First TT Injection")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Second TT Injection"){
  #     tmp_data <- v4v5combined_scg$second_tt_injection_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Second TT Injection")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "First Prenatal Visit"){
  #     tmp_data <- v4v5combined_scg$first_prenatal_visit_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("First Prenatal Visit")
  #     ggplotly(tmp)
  #   }  else if(input$distributions == "Second Prenatal Visit"){
  #     tmp_data <- v4v5combined_scg$second_prenatal_visit_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Second Prenatal Visit")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Third Prenatal Visit"){
  #     tmp_data <- v4v5combined_scg$third_prenatal_visit_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Third Prenatal Visit")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Fourth Prenatal Visit"){
  #     tmp_data <- v4v5combined_scg$fourth_prenatal_visit_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Fourth Prenatal Visit")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Child Due Date Recall"){
  #     tmp_data <- v4v5combined_scg$ch_due_date_recall_1_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Child Due Date Recall")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Recall TT1"){
  #     tmp_data <- v4v5combined_scg$recall_TT1_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Recall TT1")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Recall TT2"){
  #     tmp_data <- v4v5combined_scg$recall_TT2_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Recall TT2")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Pre Natal Check Up How Many"){
  #     tmp_data <- v4v5combined_scg$prenat_checkup_how_many_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Pre Natal Check Up How Many")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Months Pregnant"){
  #     tmp_data <- v4v5combined_scg$months_pregnant_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Months Pregnant")
  #     ggplotly(tmp)
  #   } else if(input$distributions == "Send reminder caregiver maternal home"){
  #     tmp_data <- v4v5combined_scg$send_rem_cg_mat_home_1
  #     tmp_data <- tmp_data[!is.na(tmp_data)]
  #     tmp <- qplot(as.factor(tmp_data)) + xlab("Send reminder caregiver maternal home")
  #     ggplotly(tmp)
  #   }     
  # })
  
}


shinyApp(ui = ui, server = server)
