## function rank_hospital takes 3 arguments: state, outcome, and num. The function reads
## the outcome-of-care-measures.csv file and returns a character vector with the 
## name of the hospital that has the ranking specified by the num argument 

rank_hospital <- function(State, Outcome, Num){
  ########set the working directory 
  setwd("~/Desktop/Summer school/Data science [Specialization]/R programming /Week 4/rprog_data_ProgAssignment3-data")
  library(magrittr)
  library(dplyr)
  ########read outcome data 
  csv <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, 
                      header = TRUE, na.strings = "Not Available") ##reading the data 
  table <- data.frame(csv [, 2], #"hospital"
                      csv [, 7], #"state"
                      csv [, 11]) #"heart attack"
  colnames(table) <- c("hospital", "state", "heart attack") ##selecting columns 
  table_1 <- table[!(table$`heart attack` == "Not Available"), ] ##changing Not Avilables to NA 
  table_2 <- na.omit(table_1) ##removing all the NAs 
  ########check the state and outcome are valid 
  if(!State %in% table_2$state){
    stop('invalid state')
  }
  if(!Outcome %in% "heart attack"){
    stop('invalid outcome')
  }
  ########Return hospital name in that state with the given rank 
  #first select the state, here we use subset to isolate the rows 
  table_3 <- filter(table_2, table_2$state == State)
  #sort the new table by heart attack value from smallest to largest 
  table_4 <- table_3 %>% arrange(`heart attack`)
          #print(table_4) used to test we're on track
  #use order function to rearrange their row positions alphabetically based on hospital names
  #here the order function takes in 2 variables, indicating that it should first order by heart attack 
  #values and if there's tie, rank by hospital name alphabetically 
  table_5 <- table_4[order(table_4$`heart attack`, table_4$hospital), ]
          #print(table_5)
  #use rank() function to Creating a column with Ranks of values! 
  table_5$Rank <- rank(table_5$`heart attack`, ties.method = "first")
          #print(table_5) 
  #select the correct roles. 
  for (i in Num){
    if (i=='best') {
      table_6 <- head(table_5, n = 1)
    } else if (i=='worst') {
      table_6 <- tail(table_5, n = 1)
    } else {
      table_6 <- table_5[table_5$Rank == Num, ]
    }
  }
          #print(table_6)
  ######### Return hospital name 
  print(table_6[1, 1]) 
}
