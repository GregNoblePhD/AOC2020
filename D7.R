
library(data.table)
library(dplyr)
library(stringr)


Input = readLines('C:/Users/noble/Desktop/AOC 2020/D7P1 Data.txt')
Input = data.table(Input)
setnames(Input, 'Value')

#Part 1
bag_list = strsplit(Input$Value, split = 'contain')

x = data.table(cbind('bag' = sapply(bag_list, "[[", 1), 'contents' = sapply(bag_list, "[[", 2)))
x$bag_mod = ifelse(substr(x$bag, nchar(x$bag)-1, nchar(x$bag)) == 's ', substr(x$bag, 1, nchar(x$bag) - 2), x$bag)
Cur_Bag_list = 'shiny gold'
temp_count = 0
#for(iBag in Cur_Bag_list){
while(length(Cur_Bag_list) > 0){
  iBag = Cur_Bag_list[1]
  temp_results = x[str_detect(x$contents, iBag, negate = FALSE)]
  temp_count = temp_count + nrow(temp_results)
  temp_list = c(Cur_Bag_list, temp_results$bag_mod)
  Cur_Bag_list = temp_list[!temp_list %in% iBag]
  x = x[str_detect(x$contents, iBag, negate = TRUE)]
}

temp_count


#Part 2
x = data.table(cbind('bag' = sapply(bag_list, "[[", 1), 'contents' = sapply(bag_list, "[[", 2)))
x$bag_mod = ifelse(substr(x$bag, nchar(x$bag)-1, nchar(x$bag)) == 's ', substr(x$bag, 1, nchar(x$bag) - 2), x$bag)
Cur_Bag_list = 'shiny gold'
result_count = 0
next_list = NULL

while(length(Cur_Bag_list) > 0){
  iBag = Cur_Bag_list[1]
  temp_results = x[str_detect(x$bag_mod, iBag, negate = FALSE)]
  if(temp_results$contents != " no other bags."){
    contents_list = unlist(strsplit(temp_results$contents, split = ','))
    contents_list = str_trim(contents_list, side = c("both"))
    new_list = data.table(cbind('Value' = contents_list, 
                     'count' = substr(contents_list,1,1), 
                     'bag' = substr(contents_list, 3, nchar(contents_list)-1))
                     )
    new_list$bag = ifelse(substr(new_list$bag, nchar(new_list$bag), nchar(new_list$bag)) == 's', 
                          substr(new_list$bag, 1, nchar(new_list$bag)-1),
                                 new_list$bag
                          )
    
    new_list$bag = ifelse(substr(new_list$bag, nchar(new_list$bag), nchar(new_list$bag)) == 'a', 
                          paste0(new_list$bag, 'g'),
                          new_list$bag
    )
    new_list$count = as.numeric(new_list$count)
    
    if(iBag %in% next_list$bag){
      new_list$count = new_list$count * sum(next_list$count[next_list$bag == iBag])
      
    }
    
    next_list = rbind(next_list, new_list)
    result_count = result_count + sum(next_list$count[next_list$bag %in% iBag])
    next_list = next_list[!next_list$bag %in% iBag,]
    
    Cur_Bag_list = next_list$bag
  }
  
  if(temp_results$contents == " no other bags."){
    result_count = result_count + sum(next_list$count[temp_results$bag_mod == next_list$bag])
    next_list = next_list[!next_list$bag %in% iBag,]
    Cur_Bag_list = next_list$bag
  }
  
}

result_count
#7872






