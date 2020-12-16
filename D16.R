#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Part 1
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D16P1 Data.txt')

Rules = Input[1:(which(Input == 'your ticket:') - 2)]
Rules = sapply(strsplit(Rules, split = ':'), "[[", 2)
Rules = unlist(strsplit(Rules, split = 'or'))
Rules = gsub(" ", "", Rules, fixed = TRUE)
Rules = strsplit(Rules, split = '-')


Valid = NULL
for(nLine in 1:length(Rules)){
  Valid = c(Valid, as.numeric(Rules[[nLine]][1]):as.numeric(Rules[[nLine]][2]))
}

NB_Tick = Input[(1+which(Input == 'nearby tickets:')):length(Input)]
NB_Tick = as.numeric(unlist(strsplit(NB_Tick, split = ',')))

Invalid = NB_Tick[which(!(NB_Tick %in% Valid))]

sum(Invalid)

Tick_Remove = data.table(cbind('Index' = 1:length(NB_Tick), 'Value' = NB_Tick))
Tick_Remove = ceiling(Tick_Remove$Index[which(Tick_Remove$Value %in% Invalid)] / 20)



#Part 2
#Read in the data
Input = readLines('C:/Users/noble/Desktop/AOC 2020/D16P1 Data.txt')

Rules = Input[1:(which(Input == 'your ticket:') - 2)]
Rule_Fields = sapply(strsplit(Rules, split = ':'), "[[", 1)
Rule_Values = sapply(strsplit(Rules, split = ':'), "[[", 2)
Rule_Values = unlist(strsplit(Rule_Values, split = 'or'))
Rule_Values = gsub(" ", "", Rule_Values, fixed = TRUE)
Rule_Values = strsplit(Rule_Values, split = '-')


Valid = NULL
for(nLine in 1:length(Rule_Values)){
  Valid = rbindlist(list(Valid, data.table('Rule' = Rule_Fields[ceiling(nLine/2)], 
                                   'Min_Value' = as.numeric(Rule_Values[[nLine]][1]),
                                   'Max_Value' = as.numeric(Rule_Values[[nLine]][2]))))
}

My_Tick = Input[(which(Input == 'your ticket:') +1)]
All_Ticks = c(My_Tick, Input[(1+which(Input == 'nearby tickets:')):length(Input)])
All_Ticks = strsplit(All_Ticks, split = ',')
All_Ticks = All_Ticks[-(Tick_Remove+1)]

Result = NULL
for(iPos in 1:length(All_Ticks[[1]])){
  Pos_Val = as.numeric(sapply(All_Ticks, "[[", iPos))
  for(iRule in Rule_Fields){
    temp_valid = Valid[Rule == iRule]
    pos_found = all(Pos_Val %in% c(temp_valid$Min_Value[1]:temp_valid$Max_Value[1], 
                                     temp_valid$Min_Value[2]:temp_valid$Max_Value[2]))
    Pos_Val[!(Pos_Val %in% c(temp_valid$Min_Value[1]:temp_valid$Max_Value[1], 
                   temp_valid$Min_Value[2]:temp_valid$Max_Value[2]))]
    if(pos_found){
      Result = rbindlist(list(Result, data.table(iRule, iPos)))
      print(paste0(iPos, ', ', iRule))

    } 
  }
}

Sum_Result = data.table(Result %>%
             group_by(iPos) %>%
             mutate(n = n()))

View(Sum_Result)
x = c(3,17, 6, 2, 18, 14)
prod(My_Tick_Result[x])  















