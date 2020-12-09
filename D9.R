#libraries
library(data.table)
library(dplyr)

#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D9P1 Data.txt', header = F)
setnames(Input, c('Value'))

#Part 1
Start_point = 25
for(pt in Start_point:nrow(Input)){
  temp_target = Input$Value[pt+1]
  temp_comb = data.table(expand.grid(Input$Value[(pt-Start_point+1):pt], Input$Value[(pt-Start_point+1):pt]))
  temp_comb$Sum_row = temp_comb$Var1 + temp_comb$Var2
  temp_comb$Same_Check = ifelse(temp_comb$Var1 == temp_comb$Var2, 1, 0)
  temp_comb = temp_comb[temp_comb$Same_Check == 0,]
  if(!(any(temp_comb$Sum_row == temp_target))){
    break
  }
}

temp_target


#part 2
endrow = which(Input$Value == temp_target)
for(ipt in 1:(endrow-1)){
  jpt = ipt + 1
  temp_sum = 0
  while(temp_sum < temp_target){
    temp_sum = sum(Input$Value[ipt:jpt])
    if(temp_sum == temp_target){break}
    jpt = jpt + 1
  }
  if(temp_sum == temp_target){break}
}

min(Input$Value[ipt:jpt]) + max(Input$Value[ipt:jpt])
















