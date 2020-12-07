
library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2020/D3P1 Data.txt', header = F)
setnames(Input, c('Pattern'))

#Part 1 & 2
Down_Val = 2
Right_Val = 1

row_len = nchar(Input$Pattern[1])
X_seq = seq(1, nrow(Input)*10, by = Right_Val)
Y_seq = seq(1, nrow(Input), by = Down_Val)

Input[, Row_Num := 1:nrow(Input)]
Input = Input[Row_Num %in% Y_seq]
Input = cbind(Input, 'x_seq' = X_seq[1:nrow(Input)], 'y_seq' = Y_seq)
Input[, Mod_Val := Input$x_seq %% row_len]
Input[, Mod_Val := ifelse(Input$Mod_Val == 0, row_len, Input$Mod_Val)]
Input[, Value := substr(Input$Pattern, Mod_Val, Mod_Val)]
Input[, Val_Count := ifelse(Input$Value == '#', 1, 0)]
sum(Input$Val_Count)


#Part 2
#Right 1, down 1.  #79
#Right 3, down 1. (This is the slope you already checked.)  #216
#Right 5, down 1. #91
#Right 7, down 1. #96
#Right 1, down 2. #45
79*216*91*96*45
