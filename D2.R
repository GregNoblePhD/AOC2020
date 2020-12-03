
library(data.table)
library(stringr)

Input = fread('C:/Users/noble/Desktop/AOC 2020/D2P1 Data.txt', header = F)
setnames(Input, c('Range', 'Target', 'PW'))

#Part 1
Input[, Target := substr(Target, 1, 1)]
Input[, Min_Range := as.numeric(sub("\\-.*", "", Input$Range))]
Input[, Max_Range := as.numeric(sub(".*-", "", Input$Range))]

Input$Valid = ifelse(str_count(Input$PW, Input$Target) <= Input$Max_Range & str_count(Input$PW, Input$Target) >= Input$Min_Range,1,0)

sum(Input$Valid)


#Part 2
Input$Valid_1 = ifelse(substr(Input$PW, Input$Min_Range, Input$Min_Range) == Input$Target, 1, 0)
Input$Valid_2 = ifelse(substr(Input$PW, Input$Max_Range, Input$Max_Range) == Input$Target, 1, 0)
Input$Valid_Sum = Input$Valid_1 + Input$Valid_2
Input$Final_Valid = ifelse(Input$Valid_Sum == 1, 1, 0)

sum(Input$Final_Valid)


