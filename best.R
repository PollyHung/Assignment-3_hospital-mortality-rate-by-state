best <- function(x, y){
  ## set the working directory and library the needed packages 
  setwd("~/Desktop/rprog_data_ProgAssignment3-data")
  library(magrittr)
  library(dplyr)
  ## Read outcome data 
  outcome <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, 
                      header = TRUE, na.strings = "Not Available") ##reading the data 
  table <- data.frame(outcome [, 2], #"hospital"
                         outcome [, 7], #"state"
                         outcome [, 11]) #"heart attack"
  colnames(table) <- c("hospital", "state", "heart attack") ##selecting columns 
  table_1 <- table[!(table$`heart attack` == "Not Available"), ] ##changing Not Avilables to NA 
  table_2 <- na.omit(table_1) ##removing all the NAs
  ## Check the state and outcome are valid 
  if(!x %in% table_2$state){
    stop('invalid state')
  }
  if(!y %in% "heart attack"){
    stop('invalid outcome')
  }
  ## Order our list by state and heart attack
  ## first select the state, here we use subset to isolate the rows 
  table_3 <- filter(table_2, table_2$state == x)
  ## then sort the new table by heart attack value from smallest to largest 
  table_4 <- table_3 %>% arrange(`heart attack`)
  ## then select the rows with minimum mortality value 
  table_5 <- filter(table_4, `heart attack` == min(`heart attack`)) %>% 
    filter(1:n() == 1)
  ## then sort the new table by alphabet of the hospital 
  table_6 <- table_5[order(rownames(table_5)), ]
  ## then select the first row (with the smallest mortality)
  table_7 <- table_6[1, ]
  ## Return hospital name 
  print(table_7[1, 1])
}

