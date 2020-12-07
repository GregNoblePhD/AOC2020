
library(data.table)
library(dplyr)

Input = fread('C:/Users/noble/Desktop/AOC 2020/D5P1 Data.txt', header = F)
setnames(Input, c('Value'))


#Part 1
Row_Count = 128  #0 through 127
Col_Count = 8

Result = matrix(data = rep(1, Row_Count * nrow(Input)), nrow = Row_Count, ncol = nrow(Input))

Max_Seat = lapply(Input$Value, function(x){
  temp_min = 0
  temp_max = Row_Count - 1
  temp_c_min = 0
  temp_c_max = Col_Count - 1
  for(i in 1:nchar(x)){
  #for(i in 1:7){
    if(substr(x, i, i) == 'F'){
      temp_max = floor(temp_max - 0.5 * (temp_max - temp_min))
    }
    if(substr(x, i, i) == 'B'){
      temp_min = ceiling(temp_min + 0.5 * (temp_max - temp_min))
    }
    if(substr(x, i, i) == 'L'){
      temp_c_max = floor(temp_c_max - 0.5 * (temp_c_max - temp_c_min))
    }
    if(substr(x, i, i) == 'R'){
      temp_c_min = ceiling(temp_c_min + 0.5 * (temp_c_max - temp_c_min))
    }
  }
  
  temp_max * 8 + temp_c_max
  
})
Final_Result = data.table(cbind(Max_Seat))
max(unlist(Final_Result$Max_Seat))


#Part 2
Final_Result = data.table(unlist(Final_Result))
Ordered_Results = Final_Result$V1[order(Final_Result$V1)]
key = Ordered_Results - c(Ordered_Results[2:length(Ordered_Results)],0)
which(key == -2)





