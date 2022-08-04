###rankall takes 2 arguments outcome name (outcome) and hospital rank (num). returns 2 colun data frame containing the hospital in each state that has 
###the ranking specified in num. the function should return a value for every state 

#...........ᕕ( ᐛ )ᕗ__________________________________________________________________________________________________________________________________
rankall <- function(outcome, num="best") {
#......................ᕕ( ᐛ )ᕗ
  #set the working directory 
  setwd("~/Desktop/Summer school/Data science [Specialization]/R programming /Week 4/rprog_data_ProgAssignment3-data")
  library(magrittr)
  library(dplyr)
#......................................ᕕ( ᐛ )ᕗ_______________________________________________________________________________________________________
  ########read outcome data 
  csv <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, 
                  header = TRUE, na.strings = "Not Available") ##reading the data 
  table <- data.frame(csv [, 2], #"hospital"
                      csv [, 7], #"state"
                      csv [, 11]) #"heart attack"
  colnames(table) <- c("hospital", "state", "heart attack") ##selecting columns 
  table_1 <- table[!(table$`heart attack` == "Not Available"), ] ##changing Not Avilables to NA 
  table_2 <- na.omit(table_1) ##removing all the NAs 
#............................................................ᕕ( ᐛ )ᕗ_________________________________________________________________________________
  ########check the state and outcome are valid 
  if(!outcome %in% "heart attack"){
    stop('invalid outcome')
  }
#...............................................................................ᕕ( ᐛ )ᕗ______________________________________________________________
  #Return hospital name in that state with the given rank & preparation before for-loop
  #create a list of states to iterate over. 
  state_list = sort(unique(table_2$state)) #remove the repetitive state names, sort the 54 state names alphabetically
  state_list = as.list(state_list) #we created a list of 54 
  #re-order the list via priority of state (alphabetically) -> heart attack -> hospital 
  table_3 <- table_2[order(table_2$state, table_2$`heart attack`, table_2$hospital), ] 
  #n is a large list with 54 elements, each element is a data frame with nrows and 3 columns 
  new_list <- split(table_3, table_3$state) 
  #create an empty vector
  o <- vector()
#..................................................................................................ᕕ( ᐛ )ᕗ___________________________________________
  #use for loop to iterate over each state, split the table by state, and extract the rank 1 hospital (i.e., row 1)
  #for (i in state_list) {print(i)} iterates over the 50 state names 
  for (i in 1:length(state_list)) {
    #print(i)
    n <- as.data.frame(new_list[i])
    m <- n[1, ]
    o[i] <- m
  }
#...........................................................................................................................ᕕ( ᐛ )ᕗ__________________
  #turn these two lists into table columns 
  hospital <- data.frame(Reduce(rbind, o))
  #print(hospital)
  state <- data.frame(Reduce(rbind, state_list))
  #print(state)
  #coerce the column 
  final <- cbind(hospital, state)
  colnames(final) <- c("hospital", "state")
#..............................................................................................................................................ᕕ( ᐛ )ᕗ
  print(final)
}
