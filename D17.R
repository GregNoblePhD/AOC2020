#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)


#Part 1
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D17P1 Data.txt', header = F)
setnames(Input, c('Value'))

row_len = nchar(Input$Value[1])
col_len = nrow(Input)

Input_mat = data.table(t(matrix(unlist(strsplit(Input$Value, '')), nrow = col_len, ncol = row_len)))
Setup_DT = NULL
for(iRow in 1:row_len){
  for(iCol in 1:col_len){
    Setup_DT = rbindlist(list(Setup_DT, data.table('X' = iRow, 'Y' = iCol, 'Z' = 0, 'Value' = Input_mat[iRow, iCol, with = F][1])))
  }
}
setnames(Setup_DT, c('X', 'Y', 'Z', 'Value'))
Setup_DT$Value = as.numeric(ifelse(Setup_DT$Value == '.', 0, 1))

Current_DT = data.table(expand.grid('X' = -7:7, 'Y' = -7:7, 'Z' = -5:5))
Current_DT = merge(Current_DT, Setup_DT, by = c('X', 'Y', 'Z'), all.x = TRUE)
Current_DT$Value[is.na(Current_DT$Value)] = 0

#Iterate for 6 steps
for (j in 1:6){
  
  #Find all additional Neighbors
  All_NB = NULL
  Current_Active = Current_DT[Value == 1]
  #Current_Active = Current_DT
  for(iPT in 1:nrow(Current_Active)){
    temp_pt = Current_Active[iPT,]
    temp_Val = Current_Active$Value[iPT]
    temp_grid = expand.grid('X' = (temp_pt$X - 1):(temp_pt$X + 1), 'Y' = (temp_pt$Y - 1):(temp_pt$Y + 1), 'Z' = (temp_pt$Z - 1):(temp_pt$Z + 1)) 
    temp_NB = merge(temp_grid, Current_Active, by = c('X', 'Y', 'Z'), all.x = TRUE)
    temp_NB$Value[is.na(temp_NB$Value)] = 0
    All_NB = rbindlist(list(All_NB, temp_NB))
  }
  All_NB = unique(All_NB)
  
  
  #update values for all neighbors
  New_PTs = NULL
  for(iPT in 1:nrow(All_NB)){
    temp_pt = All_NB[iPT,]
    temp_Val = All_NB$Value[iPT]
    temp_grid = expand.grid('X' = (temp_pt$X - 1):(temp_pt$X + 1), 'Y' = (temp_pt$Y - 1):(temp_pt$Y + 1), 'Z' = (temp_pt$Z - 1):(temp_pt$Z + 1)) 
    temp_NB = merge(temp_grid, All_NB, by = c('X', 'Y', 'Z'), all.x = TRUE)
    temp_NB = temp_NB[-(which(temp_NB$X == temp_pt$X & temp_NB$Y == temp_pt$Y & temp_NB$Z == temp_pt$Z)),]
    temp_NB$Value[is.na(temp_NB$Value)] = 0
    if(temp_Val == 1){
      new_Val = ifelse(sum(temp_NB$Value) == 2 | sum(temp_NB$Value) == 3, 1, 0)
    } else {
      new_Val = ifelse(sum(temp_NB$Value) == 3, 1, 0)
    }
    temp_pt$Value = new_Val
    New_PTs = rbindlist(list(New_PTs, temp_pt))
  }
  
  #Update the current state
  Current_DT = New_PTs
  
  #last one printed is result
  print(sum(New_PTs$Value))
}




#Part 2
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D17P1 Data.txt', header = F)
setnames(Input, c('Value'))

row_len = nchar(Input$Value[1])
col_len = nrow(Input)

Input_mat = data.table(t(matrix(unlist(strsplit(Input$Value, '')), nrow = col_len, ncol = row_len)))
Setup_DT = NULL
for(iRow in 1:row_len){
  for(iCol in 1:col_len){
    Setup_DT = rbindlist(list(Setup_DT, data.table('X' = iRow, 'Y' = iCol, 'Z' = 0, 'W' = 0, 'Value' = Input_mat[iRow, iCol, with = F][1])))
  }
}
setnames(Setup_DT, c('X', 'Y', 'Z', 'W', 'Value'))
Setup_DT$Value = as.numeric(ifelse(Setup_DT$Value == '.', 0, 1))


Current_DT = data.table(expand.grid('X' = -7:7, 'Y' = -7:7, 'Z' = -5:5, 'W' = -5:5))
Current_DT = merge(Current_DT, Setup_DT, by = c('X', 'Y', 'Z', 'W'), all.x = TRUE)
Current_DT$Value[is.na(Current_DT$Value)] = 0

#Iterate for 6 steps
for (j in 1:6){
  
  #Find all additional Neighbors
  All_NB = NULL
  Current_Active = Current_DT[Value == 1]
  for(iPT in 1:nrow(Current_Active)){
    temp_pt = Current_Active[iPT,]
    temp_Val = Current_Active$Value[iPT]
    temp_grid = expand.grid('X' = (temp_pt$X - 1):(temp_pt$X + 1), 'Y' = (temp_pt$Y - 1):(temp_pt$Y + 1), 'Z' = (temp_pt$Z - 1):(temp_pt$Z + 1), 'W' = (temp_pt$W - 1):(temp_pt$W + 1)) 
    temp_NB = merge(temp_grid, Current_Active, by = c('X', 'Y', 'Z', 'W'), all.x = TRUE)
    temp_NB$Value[is.na(temp_NB$Value)] = 0
    All_NB = rbindlist(list(All_NB, temp_NB))
  }
  All_NB = unique(All_NB)
  
  #update values for all neighbors
  New_PTs = NULL
  for(iPT in 1:nrow(All_NB)){
    temp_pt = All_NB[iPT,]
    temp_Val = All_NB$Value[iPT]
    temp_grid = expand.grid('X' = (temp_pt$X - 1):(temp_pt$X + 1), 'Y' = (temp_pt$Y - 1):(temp_pt$Y + 1), 'Z' = (temp_pt$Z - 1):(temp_pt$Z + 1), 'W' = (temp_pt$W - 1):(temp_pt$W + 1)) 
    temp_NB = merge(temp_grid, All_NB, by = c('X', 'Y', 'Z', 'W'), all.x = TRUE)
    temp_NB = temp_NB[-(which(temp_NB$X == temp_pt$X & temp_NB$Y == temp_pt$Y & temp_NB$Z == temp_pt$Z & temp_NB$W == temp_pt$W)),]
    temp_NB$Value[is.na(temp_NB$Value)] = 0
    if(temp_Val == 1){
      new_Val = ifelse(sum(temp_NB$Value) == 2 | sum(temp_NB$Value) == 3, 1, 0)
    } else {
      new_Val = ifelse(sum(temp_NB$Value) == 3, 1, 0)
    }
    temp_pt$Value = new_Val
    New_PTs = rbindlist(list(New_PTs, temp_pt))
  }
  
  #Update the current state
  Current_DT = New_PTs
  
  #last one printed is result
  print(sum(New_PTs$Value))
}







