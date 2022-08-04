best_1 <- function(state_name, outcome_name){
  ## set the working directory and library the needed packages 
  setwd("~/Desktop/Summer school/Data science [Specialization]/R programming /Week 4/rprog_data_ProgAssignment3-data")
  library(magrittr)
  library(dplyr)
  ## Read outcome data 
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, 
                      header = TRUE, na.strings = "Not Available") ##reading the data 
  table <- data.frame(outcome [, 2], #hospital
                      outcome [, 7], #state
                      outcome [, 11], #heart attack
                      outcome [, 17], #heart failure
                      outcome [, 23]) #Pneumonia
  colnames(table) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") ##selecting columns 

  ## Check the state and outcome are valid 
  if(!state_name %in% table$state){
    stop('invalid state')
  }
  if(!outcome_name %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ##changing Not Avilables to NA 
  if (outcome_name == "heart attack"){
    table_1 <- table[, 1:3]
    table_2 <- table_1[!(table_1$`heart attack` == "Not Available"), ]
    table_3 <- na.omit(table_2)
    table_4 <- table_3[order(table_3$state, table_3$`heart attack`, table_3$hospital), ]
  } else if (outcome_name == "heart failure") {
    table_1 <- table[, c(1, 2, 4)]
    table_2 <- table_1[!(table_1$`heart failure` == "Not Available"), ]
    table_3 <- na.omit(table_2)
    table_4 <- table_3[order(table_3$state, table_3$`heart failure`, table_3$hospital), ]
  } else if (outcome_name == "pneumonia") {
    table_1 <- table[, c(1, 2, 5)]
    table_2 <- table_1[!(table_1$pneumonia == "Not Available"), ]
    table_3 <- na.omit(table_2)
    table_4 <- table_3[order(table_3$state, table_3$pneumonia, table_3$hospital), ]
  }
  
  ## Order our list by state and heart attack
  # first select the state, here we use subset to isolate the rows 
  table_5 <- filter(table_4, table_4$state == state_name)
  # then select the first row (with the smallest mortality)
  table_6 <- table_5[1, ]
  # Return hospital name 
  print(table_6[1, 1])
}

