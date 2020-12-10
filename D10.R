#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Read in the data0
Input = fread('C:/Users/noble/Desktop/AOC 2020/D10P1 Data.txt', header = F)
setnames(Input, c('Value'))

#Part 1
Input = Input[order(Value),]
x = Input$Value - c(0, Input$Value[1:(nrow(Input)-1)])
(length(x[x == 3]) + 1) * length(x[x == 1])


#Part 2
y = data.table(
      Input %>%
      mutate(Til_Next = Input$Value - c(0, Input$Value[1:(nrow(Input)-1)]),
             From_Next = c(Input$Value[2:(nrow(Input))], 0) - Input$Value,
             Prior_Three = ifelse(lead(Til_Next, n=1) == 3, 1, 0),
             Prior_Three = ifelse(is.na(Prior_Three), 1, Prior_Three),
             Group_sum = cumsum(Prior_Three)
             )
    )

#Remove the values that are required to stay
y2 = y[Til_Next == 1 & From_Next == 1]

#Determine group size of each unnecessary group and calculate number of combination size for each group
y2 = data.table(
         y2 %>%
           group_by(Group_sum) %>%
           summarize(Group_n = n(),
                  Combos = ((Group_n + 1)*Group_n/2) + 1
                  )
)

#answer is the product of the number of combinations in each group (which is a really big number)
prod(y2$Combos)





