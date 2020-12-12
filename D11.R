#options
options(scipen = 1000) #needed this to remove scientific notation

#libraries
library(data.table)
library(dplyr)

#Part 1
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D11P1 Data.txt', header = F)
setnames(Input, c('Value'))

row_len = nchar(Input[1,])
col_len = nrow(Input)

Input_mat = data.table(matrix(unlist(strsplit(Input$Value, '')), nrow = row_len, ncol = col_len))
seats = which(Input_mat == 'L')

x = data.table('Seats' = seats,
               'Adj_up' = ifelse(seats <= row_len, 0, seats - row_len),
               'Adj_down' = ifelse(seats >= col_len * row_len - row_len + 1, 0, seats + row_len),
               'Adj_right' = ifelse(seats %% row_len == 0, 0, seats + 1),
               'Adj_left' = ifelse(seats %% row_len == 1, 0, seats - 1),
               'Adj_UL' = seats - ifelse(seats <= row_len, row_len^3, row_len) - ifelse(seats %% row_len == 1, row_len^3, 1),
               'Adj_UR' = seats - ifelse(seats <= row_len, row_len^3, row_len) + ifelse(seats %% row_len == 0, -row_len^3, 1),
               'Adj_DL' = seats + ifelse(seats >= col_len * row_len - row_len + 1, -row_len^3, row_len) - ifelse(seats %% row_len == 1, row_len^3, 1),
               'Adj_DR' = seats + ifelse(seats >= col_len * row_len - row_len + 1, -row_len^3, row_len) + ifelse(seats %% row_len == 0, -row_len^3, 1))



Seats_filled = data.table('Seats' = seats, 'Filled' = 0)

stable = 0
while (stable == 0){
  Last_seats_filled =  Seats_filled$Filled 
  temp_seats = data.table('Seats' = x$Seats,
                          'Adj_up' = ifelse(x$Adj_up %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_down' = ifelse(x$Adj_down %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_right' = ifelse(x$Adj_right %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_left' = ifelse(x$Adj_left %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_UL' = ifelse(x$Adj_UL %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_UR' = ifelse(x$Adj_UR %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_DL' = ifelse(x$Adj_DL %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_DR' = ifelse(x$Adj_DR %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0)
  )
  Seats_filled$Filled = ifelse(rowSums(temp_seats[, 2:9]) == 0, 1, ifelse(rowSums(temp_seats[, 2:9]) >= 4, 0, Seats_filled$Filled))
  
  if(all(Last_seats_filled == Seats_filled$Filled)) { break }
  
}

sum(Seats_filled$Filled)




#Part 2
#Read in the data
Input = fread('C:/Users/noble/Desktop/AOC 2020/D11P1 Data.txt', header = F)
setnames(Input, c('Value'))

row_len = nchar(Input[1,])
col_len = nrow(Input)

Input_mat = data.table(matrix(unlist(strsplit(Input$Value, '')), nrow = row_len, ncol = col_len))
seats = which(Input_mat == 'L')
seats2 = seats
floor = which(Input_mat == '.')

x = data.table('Seats' = seats,
               'Adj_up' = ifelse(seats <= row_len, 0, seats - row_len),
               'Adj_down' = ifelse(seats >= col_len * row_len - row_len + 1, 0, seats + row_len),
               'Adj_right' = ifelse(seats %% row_len == 0, 0, seats + 1),
               'Adj_left' = ifelse(seats %% row_len == 1, 0, seats - 1),
               'Adj_UL' = seats - ifelse(seats <= row_len, row_len^3, row_len) - ifelse(seats %% row_len == 1, row_len^3, 1),
               'Adj_UR' = seats - ifelse(seats <= row_len, row_len^3, row_len) + ifelse(seats %% row_len == 0, -row_len^3, 1),
               'Adj_DL' = seats + ifelse(seats >= col_len * row_len - row_len + 1, -row_len^3, row_len) - ifelse(seats %% row_len == 1, row_len^3, 1),
               'Adj_DR' = seats + ifelse(seats >= col_len * row_len - row_len + 1, -row_len^3, row_len) + ifelse(seats %% row_len == 0, -row_len^3, 1))


for (iSeat in seats){
  x$Adj_right[seats == iSeat] = ifelse(x$Adj_right[seats == iSeat] <= 0, 
                                       0,
                                       ifelse(length(seats[seats > iSeat & (iSeat %% row_len < seats %% row_len | seats %% row_len == 0) & seats < iSeat + row_len]) == 0,
                                              0, 
                                              min(seats[seats > iSeat & (iSeat %% row_len < seats %% row_len | seats %% row_len == 0) & seats < iSeat + row_len])
                                              )
                                       )
  
  x$Adj_left[seats == iSeat] = ifelse(x$Adj_left[seats == iSeat] <= 0, 
                                       0,
                                       ifelse(length(seats[seats < iSeat & (iSeat %% row_len > seats %% row_len | iSeat %% row_len == 0) & seats %% row_len != 0 & seats > iSeat - row_len]) == 0,
                                              0, 
                                              max(seats[seats < iSeat & (iSeat %% row_len > seats %% row_len | iSeat %% row_len == 0) & seats %% row_len != 0 & seats > iSeat - row_len])
                                       )
  ) 
  
  x$Adj_up[seats == iSeat] = ifelse(x$Adj_up[seats == iSeat] <= 0, 
                                      0,
                                      ifelse(length(seats[seats < iSeat & (iSeat %% row_len == seats %% row_len)]) == 0,
                                             0, 
                                             max(seats[seats < iSeat & (iSeat %% row_len == seats %% row_len)])
                                      )
  )
      
  x$Adj_down[seats == iSeat] = ifelse(x$Adj_down[seats == iSeat] <= 0, 
                                    0,
                                    ifelse(length(seats[seats > iSeat & (iSeat %% row_len == seats %% row_len)]) == 0,
                                           0, 
                                           min(seats[seats > iSeat & (iSeat %% row_len == seats %% row_len)])
                                    )
  )
  x$Adj_UL[seats == iSeat] = ifelse(x$Adj_UL[seats == iSeat] <= 0, 
                                      0,
                                      ifelse(length(seats[seats < iSeat & ((seats - iSeat) %% (row_len + 1) == 0)]) == 0,
                                             0, 
                                             max(seats[seats < iSeat & ((seats - iSeat) %% (row_len + 1) == 0)])
                                      )
  ) 
  x$Adj_UR[seats == iSeat] = ifelse(x$Adj_UR[seats == iSeat] <= 0, 
                                    0,
                                    ifelse(length(seats[seats < iSeat & ((seats - iSeat) %% (row_len - 1) == 0)]) == 0,
                                           0, 
                                           max(seats[seats < iSeat & ((seats - iSeat) %% (row_len - 1) == 0)])
                                    )
  )
  x$Adj_DL[seats == iSeat] = ifelse(x$Adj_DL[seats == iSeat] <= 0, 
                                    0,
                                    ifelse(length(seats[seats > iSeat & ((seats - iSeat) %% (row_len - 1) == 0)]) == 0,
                                           0, 
                                           min(seats[seats > iSeat & ((seats - iSeat) %% (row_len - 1) == 0)])
                                    )
  )
  x$Adj_DR[seats == iSeat] = ifelse(x$Adj_DR[seats == iSeat] <= 0, 
                                    0,
                                    ifelse(length(seats[seats > iSeat & ((seats - iSeat) %% (row_len + 1) == 0)]) == 0,
                                           0, 
                                           min(seats[seats > iSeat & ((seats - iSeat) %% (row_len + 1) == 0)])
                                    )
  )
}

Seats_filled = data.table('Seats' = seats, 'Filled' = 0)

stable = 0
while (stable == 0){
  Last_seats_filled =  Seats_filled$Filled 
  temp_seats = data.table('Seats' = x$Seats,
                          'Adj_up' = ifelse(x$Adj_up %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_down' = ifelse(x$Adj_down %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_right' = ifelse(x$Adj_right %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_left' = ifelse(x$Adj_left %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_UL' = ifelse(x$Adj_UL %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_UR' = ifelse(x$Adj_UR %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_DL' = ifelse(x$Adj_DL %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0),
                          'Adj_DR' = ifelse(x$Adj_DR %in% Seats_filled$Seats[Seats_filled$Filled == 1], 1, 0)
  )
  Seats_filled$Filled = ifelse(rowSums(temp_seats[, 2:9]) == 0, 1, ifelse(rowSums(temp_seats[, 2:9]) >= 5, 0, Seats_filled$Filled))
  
  if(all(Last_seats_filled == Seats_filled$Filled)) { break }
  
}

sum(Seats_filled$Filled)









