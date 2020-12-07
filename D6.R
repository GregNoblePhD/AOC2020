
library(data.table)
library(dplyr)


Input = fread('C:/Users/noble/Desktop/AOC 2020/D6P1 Data.txt', header = F)
setnames(Input, 'Value')

#Part 1
Check_Input = data.table(
  Input %>%
    mutate(Break_FL = ifelse(Input$Value == '', 1, 0),
           Break_Group = cumsum(Break_FL))
)

Check_Input = Check_Input[Value != '',]


Check_Input = data.table(
  Check_Input %>%
    group_by(Break_Group) %>%
    summarize(Full_Group = paste(Value, collapse = ""),
              Group_Count = length(unique(unlist(strsplit(Full_Group, split = ''))))
              
    )
)

sum(Check_Input$Group_Count)



#Part 2
Check_Input = data.table(
  Input %>%
    mutate(Break_FL = ifelse(Input$Value == '', 1, 0),
           Break_Group = cumsum(Break_FL))
)

Check_Input = Check_Input[Value != '',]


Check_Input = data.table(
  Check_Input %>%
    group_by(Break_Group) %>%
    summarize(n= n(),
              Full_Group = paste(Value, collapse = ""),
              Group_Count = length(unique(unlist(strsplit(Full_Group, split = '')))),
              
              
    )
)

Letter_List = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")

total_yes = 0
for (i in 1:nrow(Check_Input)){
  temp_Targ = Check_Input$n[i]
  for(j in Letter_List){
    x = length(which(unlist(strsplit(Check_Input$Full_Group[i], split = '')) == j))
    if(x == temp_Targ) {total_yes = total_yes + 1}
  }
}

total_yes

