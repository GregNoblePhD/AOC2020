
library(data.table)

Input = fread('C:/Users/noble/Desktop/AOC 2020/D1P1 Data.txt')

#Part 1
x = expand.grid(n1 = Input$V1, n2 = Input$V1)
x = data.table(x)
x$Sums = rowSums(x)
x = x[Sums == 2020,]
x$Product = x$n1 * x$n2

#Part 2
x = expand.grid(n1 = Input$V1, n2 = Input$V1, n3 = Input$V1)
x = data.table(x)
x$Sums = rowSums(x)
x = x[Sums == 2020,]
x$Product = x$n1 * x$n2 * x$n3
