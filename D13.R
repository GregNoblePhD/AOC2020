#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Part 1
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D13P1 Data.txt')

EDT = as.numeric(Input[1])
Bus_ID = data.table(unlist(strsplit(Input[2], split = ',')))
setnames(Bus_ID, 'ID')
Bus_ID$ID = as.numeric(Bus_ID$ID)

Bus_ID$Mod = EDT %% as.numeric(Bus_ID$ID)
Bus_ID$Depart = as.numeric(Bus_ID$ID) - Bus_ID$Mod + EDT

(min(Bus_ID$Depart, na.rm = T) - EDT) * Bus_ID$ID[which(Bus_ID$Depart == min(Bus_ID$Depart, na.rm = T) )]



#Part 2
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D13P1 Data.txt')

#Prep Bus Data
Bus_ID = data.table(unlist(strsplit(Input[2], split = ',')))
setnames(Bus_ID, 'ID')
Bus_ID$ID = as.numeric(Bus_ID$ID)
Bus_ID$Offset = 0:(nrow(Bus_ID)-1)

#Find set buses
Target_ID = Bus_ID[!is.na(Bus_ID$ID)]

#Define loop variables
Cur_Time = 0
Time_Jump = 1

#Loop through each bus
for(iBus in 1:nrow(Target_ID)){
  
  #While current time plus the Offset amount modulus the step size is not 0
  #then continue adding to the current time
  #When == 0, then we know we've found a spot that lines up with all previous buses according to the offset
  #This does not run for the first bus
  while((Cur_Time + Target_ID$Offset[iBus]) %% Target_ID$ID[iBus] > 0){
    Cur_Time = Cur_Time + Time_Jump
  } 
  
  #exiting the loop means a solution was found for all combined prior buses.
  #Increase the jump amount for the next bus by the current bus required time
  Time_Jump = Time_Jump * Target_ID$ID[iBus]
}

#Result is the current time when the loop ends
Cur_Time

#Note - I don't think that this is guaranteed to work unless all are prime numbers
Prime_Check = NULL
for(i in Target_ID$ID){
  Prime_Check = c(Prime_Check, ifelse(all(i %% (2:(i - 1)) != 0), 'Prime', 'Not Prime'))
}
Prime_Check





