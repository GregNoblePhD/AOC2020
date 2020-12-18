#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Part 1
#Read in the data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2020/D18P1 Data.txt'))
setnames(Input, c('Value'))

Result = 0

for(nLine in Input$Value){
  L_split = strsplit(nLine, split = '')[[1]]
  L_split = L_split[L_split != ' ']
  
  #Make paren matches
  P_open = which(L_split == "(")
  P_close = which(L_split == ")")
  P_all = c(P_open, P_close)
  Group_Match = NULL
  for(P_group in P_open[length(P_open):1]){
    temp_close = min(P_close[P_close > P_group])
    Group_Match = rbindlist(list(Group_Match, data.table('Open' = P_group, 'Close' = temp_close)))
    P_close = P_close[P_close != temp_close]
  }
  
  #Calculate inside paren
  Group_Match$Dist = Group_Match$Close - Group_Match$Open
  Group_Match = Group_Match[order(Dist)]
  
  #for(iGroup in 1:nrow(Group_Match)){
  while(any(L_split == "(") ){
    iGroup = 1
    temp_group = Group_Match[iGroup,]
    replace_Val = 0
    temp_Com = ''
    for(iVal in (Group_Match$Open[iGroup] + 1):(Group_Match$Close[iGroup] - 1)){
      Input_Char = L_split[iVal]
      if(Input_Char == '+'){
        temp_Com = '+'
      } else if(Input_Char == '*'){
        temp_Com = '*'
      } else {
        jVal = as.numeric(Input_Char)
        if(temp_Com == ''){
          replace_Val = jVal
        } else if(temp_Com == '+'){
          replace_Val = replace_Val + jVal
        } else if(temp_Com == '*'){
          replace_Val = replace_Val * jVal
        }
      }
    }
    
    #Update the vector of characters
    if(Group_Match$Open[iGroup] == 1){
      L_split = c(replace_Val, L_split[(Group_Match$Close[iGroup] + 1):length(L_split)])
    } else if(Group_Match$Close[iGroup] == length(L_split)){
      L_split = c(L_split[1:(Group_Match$Open[iGroup] - 1)], replace_Val)
    } else {
      L_split = c(L_split[1:(Group_Match$Open[iGroup] - 1)], replace_Val, L_split[(Group_Match$Close[iGroup] + 1):length(L_split)])
    }
    
    #re-define groups
    #Make paren matches
    P_open = which(L_split == "(")
    P_close = which(L_split == ")")
    P_all = c(P_open, P_close)
    Group_Match = NULL
    for(P_group in P_open[length(P_open):1]){
      temp_close = min(P_close[P_close > P_group])
      Group_Match = rbindlist(list(Group_Match, data.table('Open' = P_group, 'Close' = temp_close)))
      P_close = P_close[P_close != temp_close]
    }
    
    #Calculate inside paren
    Group_Match$Dist = Group_Match$Close - Group_Match$Open
    Group_Match = Group_Match[order(Dist)]
    
  }
  #At this point parentheses are gone
  
  #calculate value
  replace_Val = 0
  temp_Com = ''
  for(iVal in 1:length(L_split)){
    Input_Char = L_split[iVal]
    if(Input_Char == '+'){
      temp_Com = '+'
    } else if(Input_Char == '*'){
      temp_Com = '*'
    } else {
      jVal = as.numeric(Input_Char)
      if(temp_Com == ''){
        replace_Val = jVal
      } else if(temp_Com == '+'){
        replace_Val = replace_Val + jVal
      } else if(temp_Com == '*'){
        replace_Val = replace_Val * jVal
      }
    }
  }
  
  Result = Result + replace_Val

}

Result






#Part 2
#Read in the data
Input = data.table(readLines('C:/Users/noble/Desktop/AOC 2020/D18P1 Data.txt'))
setnames(Input, c('Value'))

Result = 0
n = 0
for(nLine in Input$Value){
  n = n + 1
  L_split = strsplit(nLine, split = '')[[1]]
  L_split = L_split[L_split != ' ']
  
  #Handle initial '+' values
  if(any(L_split == '*' & any(L_split == '+'))){
    Adds = which(L_split == "+")
    for (nAdd in Adds[length(Adds):1]){
      if(!(L_split[nAdd - 1] %in% c('(', ')')) & !(L_split[nAdd + 1] %in% c('(', ')'))){
        replace_Val = as.numeric(L_split[nAdd - 1]) + as.numeric(L_split[nAdd + 1])
        if(nAdd - 1 == 1){
          L_split = c(replace_Val, L_split[(nAdd + 2):length(L_split)])
        } else if(nAdd + 1 == length(L_split)){
          L_split = c(L_split[1:(nAdd - 2)], replace_Val)
        } else {
          L_split = c(L_split[1:(nAdd - 2)], replace_Val, L_split[(nAdd + 2):length(L_split)])
        }
      }
    }
  }
  
  #Make paren matches
  P_open = which(L_split == "(")
  P_close = which(L_split == ")")
  P_all = c(P_open, P_close)
  Group_Match = NULL
  for(P_group in P_open[length(P_open):1]){
    temp_close = min(P_close[P_close > P_group])
    Group_Match = rbindlist(list(Group_Match, data.table('Open' = P_group, 'Close' = temp_close)))
    P_close = P_close[P_close != temp_close]
  }
  
  #Calculate inside paren
  Group_Match$Dist = Group_Match$Close - Group_Match$Open
  Group_Match = Group_Match[order(Dist)]
  #Group_Match = Group_Match[Dist > 2,]
  
  #for(iGroup in 1:nrow(Group_Match)){
  while(any(L_split == "(") ){
    iGroup = 1
    temp_group = Group_Match[iGroup,]
    replace_Val = ifelse(temp_group$Dist[1] > 2, 0, as.numeric(L_split[temp_group$Close[1] - 1]))
    temp_Com = ''
    if(temp_group$Dist[1] > 2){
      for(iVal in (Group_Match$Open[iGroup] + 1):(Group_Match$Close[iGroup] - 1)){
        Input_Char = L_split[iVal]
        if(Input_Char == '+'){
          temp_Com = '+'
        } else if(Input_Char == '*'){
          temp_Com = '*'
        } else {
          jVal = as.numeric(Input_Char)
          if(temp_Com == ''){
            replace_Val = jVal
          } else if(temp_Com == '+'){
            replace_Val = replace_Val + jVal
          } else if(temp_Com == '*'){
            replace_Val = replace_Val * jVal
          }
        }
      }
    }
    
    #Update the vector of characters
    if(Group_Match$Open[iGroup] == 1){
      L_split = c(replace_Val, L_split[(Group_Match$Close[iGroup] + 1):length(L_split)])
    } else if(Group_Match$Close[iGroup] == length(L_split)){
      L_split = c(L_split[1:(Group_Match$Open[iGroup] - 1)], replace_Val)
    } else {
      L_split = c(L_split[1:(Group_Match$Open[iGroup] - 1)], replace_Val, L_split[(Group_Match$Close[iGroup] + 1):length(L_split)])
    }
    
    #Handle initial '+' values
    if(any(L_split == '*' & any(L_split == '+'))){
      Adds = which(L_split == "+")
      for (nAdd in Adds[length(Adds):1]){
        if(!(L_split[nAdd - 1] %in% c('(', ')')) & !(L_split[nAdd + 1] %in% c('(', ')'))){
          replace_Val = as.numeric(L_split[nAdd - 1]) + as.numeric(L_split[nAdd + 1])
          if(nAdd - 1 == 1){
            L_split = c(replace_Val, L_split[(nAdd + 2):length(L_split)])
          } else if(nAdd + 1 == length(L_split)){
            L_split = c(L_split[1:(nAdd - 2)], replace_Val)
          } else {
            L_split = c(L_split[1:(nAdd - 2)], replace_Val, L_split[(nAdd + 2):length(L_split)])
          }
        }
      }
    }
    
    #re-define groups
    #Make paren matches
    P_open = which(L_split == "(")
    P_close = which(L_split == ")")
    P_all = c(P_open, P_close)
    Group_Match = NULL
    for(P_group in P_open[length(P_open):1]){
      temp_close = min(P_close[P_close > P_group])
      Group_Match = rbindlist(list(Group_Match, data.table('Open' = P_group, 'Close' = temp_close)))
      P_close = P_close[P_close != temp_close]
    }
    
    #Calculate inside paren
    Group_Match$Dist = Group_Match$Close - Group_Match$Open
    Group_Match = Group_Match[order(Dist)]
    
  }
  #At this point parentheses are gone
  
  #Handle initial '+' values
  if(any(L_split == '*' & any(L_split == '+'))){
    Adds = which(L_split == "+")
    for (nAdd in Adds[length(Adds):1]){
      if(!(L_split[nAdd - 1] %in% c('(', ')')) & !(L_split[nAdd + 1] %in% c('(', ')'))){
        replace_Val = as.numeric(L_split[nAdd - 1]) + as.numeric(L_split[nAdd + 1])
        if(nAdd - 1 == 1){
          L_split = c(replace_Val, L_split[(nAdd + 2):length(L_split)])
        } else if(nAdd + 1 == length(L_split)){
          L_split = c(L_split[1:(nAdd - 2)], replace_Val)
        } else {
          L_split = c(L_split[1:(nAdd - 2)], replace_Val, L_split[(nAdd + 2):length(L_split)])
        }
      }
    }
  }
  
  #calculate value
  replace_Val = 0
  temp_Com = ''
  for(iVal in 1:length(L_split)){
    Input_Char = L_split[iVal]
    if(Input_Char == '+'){
      temp_Com = '+'
    } else if(Input_Char == '*'){
      temp_Com = '*'
    } else {
      jVal = as.numeric(Input_Char)
      if(temp_Com == ''){
        replace_Val = jVal
      } else if(temp_Com == '+'){
        replace_Val = replace_Val + jVal
      } else if(temp_Com == '*'){
        replace_Val = replace_Val * jVal
      }
    }
  }
  
  Result = Result + replace_Val
  
}

Result












