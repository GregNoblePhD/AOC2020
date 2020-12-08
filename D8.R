#libraries
library(data.table)
library(dplyr)

#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D8P1 Data.txt', header = F)
setnames(Input, c('Oper', 'Value'))

#Part 1
#Initialize tracking variables
Input$Used = 0
accu = 0
nline = 1

#Loop
while (1 == 1){
  
  #exit loop when we revist a line
  if(Input$Used[nline] == 1){break}
  
  #logic for each operation
  if(Input$Oper[nline] == 'acc'){
    accu = accu + Input$Value[nline]
    Input$Used[nline] = 1
    nline = nline + 1
  }
  
  if(Input$Oper[nline] == 'nop'){
    Input$Used[nline] = 1
    nline = nline + 1
  }
  
  if(Input$Oper[nline] == 'jmp'){
    Input$Used[nline] = 1
    nline = nline + Input$Value[nline]
  }
  
}

#answer
accu


#Part 2
#read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D8P1 Data.txt', header = F)
setnames(Input, c('Oper', 'Value'))

#create a list of rows to change (and loop through)
Input$id = 1:nrow(Input)
Input_test_list = Input$id[Input$Oper %in% c('nop', 'jmp')]

#loop through each potential change
for(nChange in Input_test_list){
  
  #change the targeted operation
  Input$Oper[Input$id == nChange] = ifelse(Input$Oper[nChange] == 'jmp', 'nop', 'jmp')
  
  #initialize tracking variables
  Input$Used = 0
  accu = 0
  nline = 1
  
  #loop 
  while (1 == 1){
    
    #break when we revist a specific row. this indicates an infinite loop
    if(Input$Used[nline] == 1){break}
    
    #logic for each operation
    if(Input$Oper[nline] == 'acc'){
      accu = accu + Input$Value[nline]
      Input$Used[nline] = 1
      nline = nline + 1
    }
    
    if(Input$Oper[nline] == 'nop'){
      Input$Used[nline] = 1
      nline = nline + 1
    }
    
    if(Input$Oper[nline] == 'jmp'){
      Input$Used[nline] = 1
      nline = nline + Input$Value[nline]
    }
  }
  
  #change the operation that was changed, back to the original to prepare for next test
  Input$Oper[Input$id == nChange] = ifelse(Input$Oper[nChange] == 'jmp', 'nop', 'jmp')
  
}

#this program will error.  when it errors the answer has been found.
#accu value when program errors is the answer.















