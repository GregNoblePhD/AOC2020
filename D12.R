#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Part 1
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D12P1 Data.txt', header = F)
setnames(Input, c('Value'))

Input$Com = substr(Input$Value, 1, 1)
Input$Quant = substr(Input$Value, 2, nchar(Input$Value))


Cur_Dir = 90
Cur_X_Pos = 0
Cur_Y_Pos = 0
for (iCom in 1:nrow(Input)){
  temp_Com = Input$Com[iCom]
  temp_Quant = as.numeric(Input$Quant[iCom])
  
  if(temp_Com == 'F'){
    if(Cur_Dir == 90){
      Cur_X_Pos = Cur_X_Pos + temp_Quant
    }
    if(Cur_Dir == 270){
      Cur_X_Pos = Cur_X_Pos - temp_Quant
    }
    if(Cur_Dir == 0){
      Cur_Y_Pos = Cur_Y_Pos + temp_Quant
    }
    if(Cur_Dir == 180){
      Cur_Y_Pos = Cur_Y_Pos - temp_Quant
    }
  }
  
  if(temp_Com == 'N'){
    Cur_Y_Pos = Cur_Y_Pos + temp_Quant
  }
  if(temp_Com == 'S'){
    Cur_Y_Pos = Cur_Y_Pos - temp_Quant
  }
  if(temp_Com == 'E'){
    Cur_X_Pos = Cur_X_Pos + temp_Quant
  }
  if(temp_Com == 'W'){
    Cur_X_Pos = Cur_X_Pos - temp_Quant
  }
  
  if(temp_Com == 'L'){
    Cur_Dir = (Cur_Dir - temp_Quant) %% 360
  }
  
  if(temp_Com == 'R'){
    Cur_Dir = (Cur_Dir + temp_Quant) %% 360
  }
  
}

abs(Cur_X_Pos) + abs(Cur_Y_Pos)





#Part 2
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D12P1 Data.txt', header = F)
setnames(Input, c('Value'))

Input$Com = substr(Input$Value, 1, 1)
Input$Quant = substr(Input$Value, 2, nchar(Input$Value))


Cur_X_Pos = 0
Cur_Y_Pos = 0
WP_X_Pos = 10
WP_Y_Pos = 1
for (iCom in 1:nrow(Input)){
  temp_Com = Input$Com[iCom]
  temp_Quant = as.numeric(Input$Quant[iCom])
  
  if(temp_Com == 'F'){
    Old_X_Pos = Cur_X_Pos
    Old_Y_Pos = Cur_Y_Pos
    Cur_X_Pos = Cur_X_Pos + temp_Quant * (WP_X_Pos - Cur_X_Pos)
    Cur_Y_Pos = Cur_Y_Pos + temp_Quant * (WP_Y_Pos - Cur_Y_Pos)
    WP_X_Pos = Cur_X_Pos + (WP_X_Pos - Old_X_Pos)
    WP_Y_Pos = Cur_Y_Pos + (WP_Y_Pos - Old_Y_Pos)
  }
  
  if(temp_Com == 'N'){
    WP_Y_Pos = WP_Y_Pos + temp_Quant
  }
  if(temp_Com == 'S'){
    WP_Y_Pos = WP_Y_Pos - temp_Quant
  }
  if(temp_Com == 'E'){
    WP_X_Pos = WP_X_Pos + temp_Quant
  }
  if(temp_Com == 'W'){
    WP_X_Pos = WP_X_Pos - temp_Quant
  }
  
  if(temp_Com == 'L'){
    if(temp_Quant == 90){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Old_WP_Y - Cur_Y_Pos)
      WP_Y_Pos = Cur_Y_Pos + (Old_WP_X - Cur_X_Pos)
    }
    if(temp_Quant == 180){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Old_WP_X - Cur_X_Pos)
      WP_Y_Pos = Cur_Y_Pos - (Old_WP_Y - Cur_Y_Pos)
    }
    if(temp_Quant == 270){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Cur_Y_Pos - Old_WP_Y)
      WP_Y_Pos = Cur_Y_Pos + (Cur_X_Pos - Old_WP_X)
    }
  }
  
  if(temp_Com == 'R'){
    if(temp_Quant == 90){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Cur_Y_Pos - Old_WP_Y)
      WP_Y_Pos = Cur_Y_Pos + (Cur_X_Pos - Old_WP_X)
    }
    if(temp_Quant == 180){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Old_WP_X - Cur_X_Pos)
      WP_Y_Pos = Cur_Y_Pos - (Old_WP_Y - Cur_Y_Pos)
    }
    if(temp_Quant == 270){
      Old_WP_X = WP_X_Pos
      Old_WP_Y = WP_Y_Pos
      WP_X_Pos = Cur_X_Pos - (Old_WP_Y - Cur_Y_Pos)
      WP_Y_Pos = Cur_Y_Pos + (Old_WP_X - Cur_X_Pos)
    }
  }
  
}

abs(Cur_X_Pos) + abs(Cur_Y_Pos)























